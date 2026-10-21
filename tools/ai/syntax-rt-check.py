#!/usr/bin/env python3
"""Checks for tools/ai/syntax-rt-progress.org (Doc 225).

Usage:
  syntax-rt-check.py plan
  syntax-rt-check.py semantics
  syntax-rt-check.py speed char-syntax|string-match
  syntax-rt-check.py final

Binaries: SYNTAX_RT_DYN / SYNTAX_RT_STATIC (default target/syntax-rt/bin/
nelisp-dyn and nelisp-static), SYNTAX_RT_BASE (default the integration-D
dynamic base).  GNU: /usr/local/bin/emacs (31.1).
"""

import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import statistics
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
DOC = ROOT / "docs/design/225-syntax-and-match-entry-gnu-parity.org"
GNU_SRC = Path("/home/madblack-21/Downloads/emacs-31.1/src")
GNU_BIN = "/usr/local/bin/emacs"
EVIDENCE = ROOT / "target/syntax-rt"
BASE_BIN = Path(os.environ.get(
    "SYNTAX_RT_BASE",
    ROOT.parent / "integ-20261021d/target/integ/nelisp-dyn"))
DYN_BIN = Path(os.environ.get("SYNTAX_RT_DYN", EVIDENCE / "bin/nelisp-dyn"))
STATIC_BIN = Path(os.environ.get("SYNTAX_RT_STATIC", EVIDENCE / "bin/nelisp-static"))
CPUS = os.environ.get("SYNTAX_RT_CPU", "10")
MEMORY_LIMIT_KB = 8388608


def sha256(path):
    digest = hashlib.sha256()
    with Path(path).open("rb") as stream:
        for block in iter(lambda: stream.read(1 << 20), b""):
            digest.update(block)
    return digest.hexdigest()


def fail(message):
    print(f"SYNTAX-RT FAIL: {message}")
    return 1


# ---------------------------------------------------------------- plan

SECTIONS = ("char-syntax ?a", "char-syntax ?(", 'string-match "\\\\sw+"',
            'string-match "alpha"')


def doc_tables(text):
    """Return {section-heading: [row-cells...]} for the §1 owner tables."""
    tables, current = {}, None
    for line in text.splitlines():
        if line.startswith("** "):
            current = line[3:].strip()
            continue
        if current and line.startswith("|") and not line.startswith("|-"):
            cells = [c.strip() for c in line.strip().strip("|").split("|")]
            if cells and cells[0] != "Owner":
                tables.setdefault(current, []).append(cells)
    return tables


def gnu_reference_ok(ref, token, shift=0):
    match = re.fullmatch(r"([\w.-]+\.[ch]):(\d+)", ref)
    if not match:
        return False
    source = GNU_SRC / match.group(1)
    if not source.is_file():
        return False
    lines = source.read_text(errors="replace").split("\n")
    line = int(match.group(2)) + shift
    window = lines[max(0, line - 4):line + 3]
    return any(token in candidate for candidate in window)


def check_plan():
    if not DOC.is_file():
        return fail(f"missing {DOC.relative_to(ROOT)}")
    text = DOC.read_text()
    tables = doc_tables(text)
    problems, refs = [], 0
    for wanted in SECTIONS:
        heading = next((h for h in tables if h.startswith(wanted)), None)
        if heading is None:
            problems.append(f"no owner table for {wanted}")
            continue
        total = 0
        for cells in tables[heading]:
            if len(cells) != 7:
                problems.append(f"{wanted}: row has {len(cells)} cells")
                continue
            owner, micros, share, mechanism, ref, token, port = cells
            if not re.fullmatch(r"[\d,]+", micros):
                problems.append(f"{wanted}: bad us/call {micros!r}")
            if not re.fullmatch(r"\d+%", share):
                problems.append(f"{wanted}: bad share {share!r}")
            else:
                total += int(share[:-1])
            if not gnu_reference_ok(ref, token):
                problems.append(f"{wanted}: {ref} does not show {token!r}")
            refs += 1
            if not re.match(r"(ported|adapted|kept|not ported)", port):
                problems.append(f"{wanted}: port decision {port!r}")
        if not 95 <= total <= 105:
            problems.append(f"{wanted}: shares sum to {total}%")
    # Negative control: the same references, shifted 40 lines, must fail.
    shifted_ok = sum(gnu_reference_ok(cells[4], cells[5], 40)
                     for rows in tables.values() for cells in rows
                     if len(cells) == 7)
    if shifted_ok:
        problems.append(f"negative control: {shifted_ok} shifted references still match")
    # The measured runs the shares come from.
    for name in ("split-base-1.txt", "split-base-2.txt"):
        run = EVIDENCE / "s0" / name
        if not run.is_file() or "(char-syntax 97) total" not in run.read_text():
            problems.append(f"missing measurement {run}")
    if BASE_BIN.is_file() and sha256(BASE_BIN) not in text:
        problems.append("base binary sha256 not cited")
    for word in ("§2", "§3", "§4", "codegen", "callpath"):
        if word not in text:
            problems.append(f"doc lacks {word}")
    if problems:
        for problem in problems:
            print(f"  {problem}")
        return fail(f"plan: {len(problems)} problem(s)")
    print(f"SYNTAX-RT plan PASS: {len(SECTIONS)} owner tables, {refs} GNU references, "
          f"negative control rejected all shifted references")
    return 0


# ---------------------------------------------------------------- speed

SPEED_PROBE = r""";; -*- lexical-binding: t; -*-
;; Own cost: the call minus a same-shape trivial subr call, both inline in a
;; dotimes loop (ten copies per iteration).  Same file for GNU and NeLisp.
(defmacro rt-loop (n form)
  `(let ((t0 (float-time)))
     (dotimes (_ ,n) ,form ,form ,form ,form ,form ,form ,form ,form ,form ,form)
     (- (float-time) t0)))
(defvar rt-n (string-to-number (or (getenv "RT_N") "1000")))
(defmacro rt-own (label scale op base)
  `(let ((n (max 1 (/ (* rt-n ,scale) 100))) xs)
     (rt-loop 20 ,op) (rt-loop 20 ,base)
     (dotimes (_ 7)
       (let* ((a (rt-loop n ,op)) (b (rt-loop n ,base)))
         (push (/ (* 1e6 (- a b)) (* 10 n)) xs)
         (push (/ (* 1e6 a) (* 10 n)) xs)
         (push (/ (* 1e6 b) (* 10 n)) xs)))
     (setq xs (nreverse xs))
     (let (own call base)
       (while xs
         (push (pop xs) own) (push (pop xs) call) (push (pop xs) base))
       (setq own (sort own #'<) call (sort call #'<) base (sort base #'<))
       (princ (format "RT %s own=%.4f call=%.4f base=%.4f\n" ,label
                      (nth 3 own) (nth 3 call) (nth 3 base))))))
"""

SPEED_CASES = {
    "char-syntax": [
        ("char-syntax-a", 100, "(char-syntax 97)", "(integerp 97)"),
        ("char-syntax-paren", 100, "(char-syntax 40)", "(integerp 40)"),
    ],
    "string-match": [
        ("string-match-plain", 20, '(string-match "alpha" "alpha")', '(eq "alpha" "alpha")'),
        ("string-match-syntax", 20, '(string-match "\\\\sw+" "alpha")', '(eq "\\\\sw+" "alpha")'),
    ],
}
# Calls per sample: GNU is fast, so it gets more iterations for resolution.
SPEED_N = {"gnu": 20000, "nelisp": 200}


def write_speed_probe(kind):
    directory = EVIDENCE / "speed"
    directory.mkdir(parents=True, exist_ok=True)
    probe = directory / f"{kind}.el"
    body = SPEED_PROBE + "".join(
        f'(rt-own "{label}" {scale} {op} {base})\n'
        for label, scale, op, base in SPEED_CASES[kind])
    probe.write_text(body)
    return probe


def run_limited(command, env=None, timeout=900):
    shell = "ulimit -v %d; exec \"$@\"" % MEMORY_LIMIT_KB
    return subprocess.run(["bash", "-c", shell, "run", *command], capture_output=True,
                          text=True, env=env, timeout=timeout, cwd=ROOT)


def speed_run(runtime, binary, probe):
    env = dict(os.environ, RT_N=str(SPEED_N[runtime]))
    command = (["taskset", "-c", CPUS, str(binary), "--load", str(probe)]
               if runtime == "nelisp"
               else ["taskset", "-c", CPUS, GNU_BIN, "-Q", "--batch", "-l", str(probe)])
    result = run_limited(command, env)
    rows = {}
    for line in result.stdout.splitlines() + result.stderr.splitlines():
        match = re.match(r"RT (\S+) own=(\S+) call=(\S+) base=(\S+)", line)
        if match:
            rows[match.group(1)] = {k: float(v) for k, v in
                                    zip(("own", "call", "base"), match.groups()[1:])}
    return result.returncode, rows


def cpu_freq():
    """The timing CPU's current clock in kHz (COMMON.md: record with every run)."""
    path = Path(f"/sys/devices/system/cpu/cpu{CPUS}/cpufreq/scaling_cur_freq")
    try:
        return int(path.read_text().strip())
    except (OSError, ValueError):
        return None


def paired_ratio(numerator, denominator):
    """NUMERATOR/DENOMINATOR for one round; a non-positive denominator (an
    own cost below the clock's resolution) makes any positive numerator
    infinitely slower and a non-positive one equal."""
    if denominator > 0:
        return numerator / denominator
    return float("inf") if numerator > 0 else 1.0


def check_speed(kind, binary=None):
    if kind not in SPEED_CASES:
        return fail(f"unknown speed case {kind}")
    binary = Path(binary or DYN_BIN)
    for required in (binary, BASE_BIN):
        if not required.is_file():
            return fail(f"missing binary {required}")
    probe = write_speed_probe(kind)
    labels = [case[0] for case in SPEED_CASES[kind]]
    rounds = []
    # Paired ratios (COMMON.md): candidate, GNU and base close together in
    # each round; the decision is the median of the per-round ratios.
    for _ in range(7):
        entry = {}
        for name, runtime, exe in (("candidate", "nelisp", binary), ("gnu", "gnu", None),
                                   ("base", "nelisp", BASE_BIN)):
            freq = cpu_freq()
            code, rows = speed_run(runtime, exe, probe)
            if code != 0 or set(rows) != set(labels):
                return fail(f"speed {kind}: {name} run failed (exit {code}, rows {sorted(rows)})")
            entry[name] = {"freq_khz": freq, "rows": rows}
        rounds.append(entry)
    record = {"kind": kind, "binary": str(binary), "binary_sha256": sha256(binary),
              "base": str(BASE_BIN), "base_sha256": sha256(BASE_BIN), "gnu": GNU_BIN,
              "cpu": CPUS, "rounds": rounds, "cases": {}}
    passed = True
    for label in labels:
        to_gnu = [paired_ratio(r["candidate"]["rows"][label]["own"], r["gnu"]["rows"][label]["own"])
                  for r in rounds]
        to_base = [paired_ratio(r["candidate"]["rows"][label]["own"], r["base"]["rows"][label]["own"])
                   for r in rounds]
        summary = {"ratio_to_gnu": statistics.median(to_gnu),
                   "ratio_to_base": statistics.median(to_base)}
        for name in ("candidate", "gnu", "base"):
            for field in ("own", "call", "base"):
                summary[f"{name}_{field}"] = statistics.median(
                    r[name]["rows"][label][field] for r in rounds)
        summary["ok"] = summary["ratio_to_gnu"] <= 1.0
        passed &= summary["ok"]
        record["cases"][label] = summary
        print(f"  {label}: own {summary['candidate_own']:.4f} us, GNU {summary['gnu_own']:.4f} us, "
              f"base {summary['base_own']:.4f} us; median paired ratio to GNU "
              f"{summary['ratio_to_gnu']:.4g}, to base {summary['ratio_to_base']:.4f} "
              f"{'PASS' if summary['ok'] else 'FAIL'}")
    freqs = [r[n]["freq_khz"] for r in rounds for n in ("candidate", "gnu", "base")
             if r[n]["freq_khz"]]
    if freqs:
        print(f"  CPU {CPUS} scaling_cur_freq {min(freqs)}-{max(freqs)} kHz over {len(freqs)} runs")
    record["passed"] = passed
    (EVIDENCE / "speed" / f"{kind}.json").write_text(json.dumps(record, indent=1) + "\n")
    if passed:
        print(f"SYNTAX-RT speed {kind} PASS")
        return 0
    return fail(f"speed {kind}: NeLisp own cost above GNU's (median paired ratio of 7 rounds)")


# ---------------------------------------------------------------- workloads

REGEX6000 = r"""(let ((names (mapcar (lambda (k) (format "some-library-%d.el" k)) (number-sequence 1 300))) (i 0))
  (while (< i 20)
    (dolist (s names) (string-match "\\`\\(.*\\)\\(\\.el\\)\\'" s))
    (setq i (1+ i))))
(princ "ABI-REGEX 6000\n")
"""
ANVIL = Path("/home/madblack-21/.emacs.d/external-packages/anvil.el")
MEMORY_DB = Path("/home/madblack-21/Cowork/Notes/.anvil-memory/anvil-memory-index.db")
MCP_QUERY = "nelisp syntax table"


def timed_process(command, env=None, marker=None, timeout=900):
    started = time.monotonic()
    result = run_limited(command, env, timeout)
    elapsed = time.monotonic() - started
    ok = result.returncode == 0 and (marker is None or marker in result.stdout)
    return ok, elapsed, result


def workload_command(name, runtime, binary):
    directory = EVIDENCE / "workloads"
    if name == "regex6000":
        script = directory / "regex6000.el"
        marker = "ABI-REGEX 6000"
    else:
        script = ROOT / "tools/ai/abi-file-exec.el"
        marker = "ABI-FILES (1000 527500"
    if runtime == "nelisp":
        command = ["taskset", "-c", CPUS, str(binary), "--load", str(script)]
    else:
        command = ["taskset", "-c", CPUS, GNU_BIN, "-Q", "--batch", "-l", str(script)]
    return command, marker


def mcp_session(binary, tag):
    """Time initialize and one memory-search over the anvil standalone MCP."""
    work = EVIDENCE / "workloads" / f"mcp-{tag}"
    if work.exists():
        shutil.rmtree(work)
    work.mkdir(parents=True)
    cache = EVIDENCE / "workloads" / "mcp-cache" / sha256(binary)[:16]
    cache.mkdir(parents=True, exist_ok=True)
    db = work / "memory.db"
    shutil.copyfile(EVIDENCE / "workloads" / "memory-snapshot.db", db)
    env = dict(os.environ, ANVIL_NOTES_DIR="/home/madblack-21/Cowork/Notes",
               ANVIL_EL_DIR=str(ANVIL), NELISP_BIN=str(binary),
               NELISP_EMACS_LIB_DIR="/home/madblack-21/Cowork/Notes/dev/nelisp-emacs-lib/src",
               ANVIL_MEMORY_DB=str(db), ANVIL_WORKLOG_DB=str(work / "worklog.db"),
               TMPDIR=str(ROOT / "target/tmp"), XDG_CACHE_HOME=str(cache))
    (ROOT / "target/tmp").mkdir(parents=True, exist_ok=True)
    shell = "ulimit -v %d; exec taskset -c %s \"$@\"" % (MEMORY_LIMIT_KB, CPUS)
    started = time.monotonic()
    process = subprocess.Popen(["bash", "-c", shell, "run", str(ANVIL / "tools/anvil-standalone-mcp.sh")],
                               stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                               stderr=(work / "stderr").open("w"), text=True, env=env, cwd=ROOT)
    record = {"tools": 0}

    def ask(request, deadline):
        process.stdin.write(json.dumps(request) + "\n")
        process.stdin.flush()
        while True:
            if time.monotonic() > deadline:
                raise TimeoutError(request["method"])
            line = process.stdout.readline()
            if not line:
                raise EOFError(request["method"])
            try:
                row = json.loads(line)
            except ValueError:
                continue
            if isinstance(row, dict) and row.get("id") == request["id"]:
                return row
    try:
        deadline = started + 300
        row = ask({"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {
            "protocolVersion": "2024-11-05", "capabilities": {},
            "clientInfo": {"name": "syntax-rt", "version": "1"}}}, deadline)
        record["initialize_s"] = time.monotonic() - started
        record["initialized"] = "result" in row
        t0 = time.monotonic()
        row = ask({"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {
            "name": "memory-search", "arguments": {"query": MCP_QUERY, "limit": "20"}}},
                  time.monotonic() + 300)
        record["search_s"] = time.monotonic() - t0
        record["search_ok"] = "result" in row and not row["result"].get("isError")
        row = ask({"jsonrpc": "2.0", "id": 3, "method": "tools/list", "params": {}},
                  time.monotonic() + 120)
        tools = row.get("result", {}).get("tools", [])
        record["tools"] = len({t.get("name") for t in tools if isinstance(t, dict)})
    except (TimeoutError, EOFError, OSError) as error:
        record["error"] = repr(error)
    finally:
        try:
            process.stdin.close()
        except OSError:
            pass
        try:
            process.wait(timeout=30)
        except subprocess.TimeoutExpired:
            process.kill()
            process.wait()
        db.unlink(missing_ok=True)
    record["ok"] = (record.get("initialized") and record.get("search_ok")
                    and record["tools"] == 40 and "error" not in record)
    return record


def run_workloads(binary=None):
    binary = Path(binary or DYN_BIN)
    directory = EVIDENCE / "workloads"
    directory.mkdir(parents=True, exist_ok=True)
    (directory / "regex6000.el").write_text(REGEX6000)
    snapshot = directory / "memory-snapshot.db"
    if not snapshot.exists():
        shutil.copyfile(MEMORY_DB, snapshot)
    env = dict(os.environ, ABI_SOURCE_ROOT=str(ROOT))
    names = ("candidate", "base")
    # Warm the MCP image caches and the file cache before timing.
    for name, exe in zip(names, (binary, BASE_BIN)):
        mcp_session(exe, f"warm-{name}")
    rounds = []
    for index in range(7):
        entry = {}
        for workload in ("regex6000", "files"):
            for name, runtime, exe in (("candidate", "nelisp", binary), ("gnu", "gnu", None),
                                       ("base", "nelisp", BASE_BIN)):
                command, marker = workload_command(workload, runtime, exe)
                freq = cpu_freq()
                ok, elapsed, _ = timed_process(command, env, marker)
                entry[f"{workload}/{name}"] = {"ok": ok, "s": elapsed, "freq_khz": freq}
        for name, exe in zip(names, (binary, BASE_BIN)):
            freq = cpu_freq()
            entry[f"mcp/{name}"] = dict(mcp_session(exe, f"{name}-{index}"), freq_khz=freq)
        rounds.append(entry)
        print(f"  round {index}: " + " ".join(
            f"{k}={v.get('s', v.get('initialize_s', 0)):.2f}" for k, v in entry.items()), flush=True)
    summary = {}
    for workload in ("regex6000", "files"):
        to_base = [r[f"{workload}/candidate"]["s"] / r[f"{workload}/base"]["s"] for r in rounds]
        to_gnu = [r[f"{workload}/candidate"]["s"] / r[f"{workload}/gnu"]["s"] for r in rounds]
        summary[workload] = {"ratio_to_base": statistics.median(to_base),
                             "ratio_to_gnu": statistics.median(to_gnu),
                             "ok": all(r[f"{workload}/{n}"]["ok"] for r in rounds
                                       for n in ("candidate", "gnu", "base"))}
    for field in ("initialize_s", "search_s"):
        values = [r["mcp/candidate"].get(field, float("inf")) /
                  r["mcp/base"].get(field, float("inf")) for r in rounds]
        summary[f"mcp/{field}"] = {"ratio_to_base": statistics.median(values),
                                   "ok": all(r[f"mcp/{n}"]["ok"] for r in rounds for n in names)}
    record = {"binary": str(binary), "binary_sha256": sha256(binary), "base": str(BASE_BIN),
              "base_sha256": sha256(BASE_BIN), "cpu": CPUS, "rounds": rounds,
              "summary": summary}
    (directory / "workloads.json").write_text(json.dumps(record, indent=1) + "\n")
    for key, value in summary.items():
        extra = f", to GNU {value['ratio_to_gnu']:.3f}" if "ratio_to_gnu" in value else ""
        print(f"  {key}: median paired ratio to base {value['ratio_to_base']:.3f}{extra} "
              f"{'ok' if value['ok'] else 'RUN FAILED'}")
    return 0


# ---------------------------------------------------------------- semantics

FIXTURE = ROOT / "test/fixtures/syntax-rt-semantics.el"
EXPECTED = ROOT / "test/fixtures/syntax-rt-semantics.gnu-31.1.expected"


CHARTAB_FIXTURE = ROOT / "test/fixtures/syntax-rt-chartab.el"
CHARTAB_EXPECTED = ROOT / "test/fixtures/syntax-rt-chartab.gnu-31.1.expected"


def run_fixture(runtime, binary=None, fixture=FIXTURE):
    command = (["taskset", "-c", CPUS, str(binary), "--load", str(fixture)]
               if runtime == "nelisp"
               else [GNU_BIN, "-Q", "--batch", "-l", str(fixture)])
    result = subprocess.run(["bash", "-c", "ulimit -v %d; exec \"$@\"" % MEMORY_LIMIT_KB,
                             "run", *command], capture_output=True, timeout=600, cwd=ROOT)
    return result.returncode, result.stdout, result.stderr


def check_fixture(label, fixture, expected_path, extra_probe=None):
    """GNU still produces the pin, dynamic and static match it byte for byte,
    and the base binary (negative control) does not."""
    for required in (fixture, expected_path, DYN_BIN, STATIC_BIN, BASE_BIN):
        if not Path(required).is_file():
            return fail(f"missing {required}")
    expected = expected_path.read_bytes()
    rows = sum(1 for line in expected.split(b"\n") if b" => " in line)
    problems, record = [], {"fixture_sha256": sha256(fixture),
                            "expected_sha256": sha256(expected_path), "rows": rows}
    code, out, err = run_fixture("gnu", fixture=fixture)
    if code != 0 or out != expected:
        problems.append("GNU 31.1 no longer produces the pinned output")
    for name, binary in (("dynamic", DYN_BIN), ("static", STATIC_BIN)):
        code, out, err = run_fixture("nelisp", binary, fixture)
        record[name] = {"binary_sha256": sha256(binary), "exit": code,
                        "stderr_bytes": len(err), "identical": out == expected}
        if code != 0 or err or out != expected:
            got = out.split(b"\n")
            bad = [line.split(b" => ")[0].decode(errors="replace")
                   for line, want in zip(got, expected.split(b"\n")) if line != want]
            problems.append(f"{name}: exit {code}, stderr {len(err)} bytes, "
                            f"{len(bad)} differing rows, first {bad[:4]}")
        if extra_probe:
            problem = extra_probe(binary)
            if problem:
                problems.append(f"{name}: {problem}")
    code, out, err = run_fixture("nelisp", BASE_BIN, fixture)
    record["base_control"] = {"binary_sha256": sha256(BASE_BIN), "exit": code,
                              "identical": out == expected}
    if code == 0 and out == expected:
        problems.append("negative control: the base binary reproduces GNU's output")
    (EVIDENCE / f"{label}.json").write_text(json.dumps(record, indent=1) + "\n")
    if problems:
        for problem in problems:
            print(f"  {problem}")
        return fail(f"{label}: {len(problems)} problem(s)")
    print(f"SYNTAX-RT {label} PASS: {rows} GNU 31.1 rows identical on dynamic and static; "
          f"the base binary differs (exit {record['base_control']['exit']})")
    return 0


def check_semantics():
    return check_fixture("semantics", FIXTURE, EXPECTED)


def native_char_table_feature(binary):
    """The library's single switch: the runtime owns the char-table family."""
    probe = EVIDENCE / "chartab-feature.el"
    probe.write_text('(princ (if (featurep (quote nelisp-native-char-tables)) "FEATURE-ON" "FEATURE-OFF"))\n'
                     "(kill-emacs 0)\n")
    result = subprocess.run(["bash", "-c", "ulimit -v %d; exec \"$@\"" % MEMORY_LIMIT_KB,
                             "run", str(binary), "--load", str(probe)],
                            capture_output=True, text=True, timeout=120, cwd=ROOT)
    if "FEATURE-ON" not in result.stdout:
        return "(featurep 'nelisp-native-char-tables) is not t"
    return None


def check_chartab():
    return check_fixture("chartab", CHARTAB_FIXTURE, CHARTAB_EXPECTED,
                         native_char_table_feature)

# ---------------------------------------------------------------- final

FINAL = EVIDENCE / "final"


def check_final():
    """S2.1: the retained final-verification evidence for the current
    binaries -- COMMON's 11 groups with 0 failures, the full standalone
    checks, and the paired workload campaign against the base (no
    regression beyond 1.03, a gain, 40 MCP tools in every session)."""
    problems = []
    for required in (DYN_BIN, STATIC_BIN, BASE_BIN):
        if not Path(required).is_file():
            return fail(f"missing {required}")
    dyn, static = sha256(DYN_BIN), sha256(STATIC_BIN)
    record_path = FINAL / "final.json"
    if not record_path.is_file():
        return fail(f"missing {record_path}; run the final verification first")
    record = json.loads(record_path.read_text())
    if record.get("dynamic_sha256") != dyn or record.get("static_sha256") != static:
        problems.append("final evidence was recorded for other binaries")
    for name, digest in record.get("files", {}).items():
        path = ROOT / name
        if not path.is_file() or sha256(path) != digest:
            problems.append(f"evidence file changed or missing: {name}")
    checks = (FINAL / "checks.log").read_text() if (FINAL / "checks.log").is_file() else ""
    if "GATE-COUNT: 11 cases, 0 failures" not in checks:
        problems.append("checks.sh did not report 11 groups with 0 failures")
    standalone = ((FINAL / "standalone-checks.log").read_text()
                  if (FINAL / "standalone-checks.log").is_file() else "")
    count = re.search(r"GATE-COUNT: (\d+) cases, (\d+) failures", standalone)
    if "STANDALONE-CHECKS PASS" not in standalone or not count or count.group(2) != "0":
        problems.append("standalone-checks did not pass")
    workloads_path = EVIDENCE / "workloads" / "workloads.json"
    workloads = json.loads(workloads_path.read_text()) if workloads_path.is_file() else {}
    if workloads.get("binary_sha256") != dyn:
        problems.append("the workload campaign was run on another binary")
    summary = workloads.get("summary", {})
    ratios = {key: value.get("ratio_to_base") for key, value in summary.items()}
    if set(ratios) != {"regex6000", "files", "mcp/initialize_s", "mcp/search_s"}:
        problems.append(f"workload campaign incomplete: {sorted(ratios)}")
    for key, value in summary.items():
        if not value.get("ok"):
            problems.append(f"{key}: a run failed (MCP needs 40 tools)")
        if value.get("ratio_to_base") is None or value["ratio_to_base"] > 1.03:
            problems.append(f"{key}: median paired ratio to base {value.get('ratio_to_base')} > 1.03")
    if not any(r is not None and r < 0.97 for r in ratios.values()):
        problems.append("no workload gains beyond the 3% spread")
    for key, ratio in sorted(ratios.items()):
        print(f"  {key}: median paired ratio to base {ratio}")
    if problems:
        for problem in problems:
            print(f"  {problem}")
        return fail(f"final: {len(problems)} problem(s)")
    print(f"SYNTAX-RT final PASS: COMMON 11 groups 0 failures, standalone-checks "
          f"{count.group(1)} cases 0 failures, workloads no regression and a gain")
    return 0


def main(argv):
    if argv[1:2] == ["final"]:
        return check_final()
    if argv[1:2] == ["workloads"] and len(argv) in (2, 3):
        return run_workloads(argv[2] if len(argv) == 3 else None)
    if argv[1:2] == ["semantics"]:
        return check_semantics()
    if argv[1:2] == ["chartab"]:
        return check_chartab()
    if argv[1:2] == ["plan"]:
        return check_plan()
    if argv[1:2] == ["speed"] and len(argv) in (3, 4):
        return check_speed(argv[2], argv[3] if len(argv) == 4 else None)
    print(__doc__.strip())
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv))
