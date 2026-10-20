#!/usr/bin/env python3
"""Run the alias corpus, record four-layout Callgrind costs, check receipts.

Measurements include GC and use two loop sizes to subtract startup. Evidence
is stored in target/prim-alias, with source and binary hashes. No machine paths
are embedded. GNU reference and candidate stdout must match byte for byte.
"""
import concurrent.futures
import bisect
import functools
import hashlib
import importlib.util
import json
import os
import re
from pathlib import Path
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/prim-alias"
BASE = OUT / "base/nelisp"
BIN = Path(os.environ.get("NELISP_BIN", OUT / "candidate/nelisp")).resolve()
PADS = (0, 700, 1400, 2100)
REFERENCES = 32
CORPUS = ROOT / "test/perf/prim-alias-corpus.el"
EXTRA = ROOT / "test/perf/prim-alias-extra.el"
FIVE_FILES = ("vendor/emacs-lisp/emacs-lisp/cl-seq.el",
              "vendor/emacs-lisp/emacs-lisp/easy-mmode.el",
              "vendor/emacs-lisp/emacs-lisp/gv.el",
              "vendor/emacs-lisp/emacs-lisp/regexp-opt.el",
              "vendor/emacs-lisp/emacs-lisp/ring.el")


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


@functools.lru_cache(maxsize=8)
def artifact_identity(binary, elf_stat, cold_stat):
    """Hash frozen artifacts once per file identity, rather than per profile."""
    return {"binary_sha256": sha(binary), "cold_sha256": sha(binary + ".cold")}


def profile_artifacts(binary):
    def identity(path):
        s = Path(path).stat()
        return (s.st_dev, s.st_ino, s.st_size, s.st_mtime_ns, s.st_ctime_ns)
    binary = str(binary)
    return artifact_identity(binary, identity(binary), identity(binary + ".cold"))


def run(argv, **kw):
    return subprocess.run(list(map(str, argv)), cwd=ROOT, capture_output=True,
                          timeout=600, **kw)


def parity():
    assert run(["emacs", "--version"]).stdout.startswith(b"GNU Emacs 31.1")
    source = OUT / "corpus.el"
    source.write_text(CORPUS.read_text() + "\n" + EXTRA.read_text() + "\n(kill-emacs 0)\n")
    outputs = []
    for label, argv in (("gnu", ["emacs", "-Q", "--batch", "-l", source]),
                        ("native", [BIN, "-L", "lisp", "-L", "src", "-L", "scripts",
                                    "-L", "packages/nl-ffi/src", "-L", "packages/nl-prelude/src",
                                    "--load", source])):
        stdout = OUT / f"{label}.stdout"
        stderr = OUT / f"{label}.stderr"
        with stdout.open("wb") as out, stderr.open("wb") as err:
            p = subprocess.run(list(map(str, argv)), cwd=ROOT, stdout=out, stderr=err, timeout=180)
        assert p.returncode == 0, (label, p.returncode, stderr.read_bytes()[:200])
        assert stdout.stat().st_size > 0, label
        outputs.append(stdout.read_bytes())
    assert outputs[0] == outputs[1], "GNU/native stdout differs; inspect target/prim-alias/*.stdout"
    assert len(outputs[0].splitlines()) == 14 + EXTRA.read_text().count("(pa-case '")
    # Warning rendering uses different blank-line spacing. Require the exact
    # GNU warning messages, including warnings preceding failed alias installs.
    assert ([x for x in (OUT / "native.stderr").read_bytes().splitlines() if x] ==
            [x for x in (OUT / "gnu.stderr").read_bytes().splitlines() if x]), "unexpected native stderr"
    host = run(["emacs", "-Q", "--batch", "--eval", r'''
(let ((original (symbol-function 'defvaralias)) (count 0))
  (with-temp-buffer
    (insert-file-contents "scripts/nelisp-stdlib-prelude.el")
    (emacs-lisp-mode)
    (check-parens)
    (goto-char (point-min))
    (search-forward "(defvar nelisp--native-variable-aliases")
    (goto-char (match-beginning 0))
    (while (progn (forward-comment (point-max)) (not (eobp)))
      (eval (read (current-buffer)) t)
      (setq count (1+ count))))
  (unless (and (>= count 2) (eq original (symbol-function 'defvaralias))
               (not (fboundp 'nelisp--native-variable-alias-pairs)))
    (error "Native alias provider leaked into GNU host"))
  (princ "ALIAS-HOST-GUARD PASS\n"))'''])
    (OUT / "host-guard.stdout").write_bytes(host.stdout)
    (OUT / "host-guard.stderr").write_bytes(host.stderr)
    assert host.returncode == 0 and host.stdout == b"ALIAS-HOST-GUARD PASS\n" and not host.stderr
    print(f"ALIAS-PARITY PASS {len(outputs[0].splitlines())} rows {sha(BIN)}")


def program(mode, access, n):
    names = [f"pa-{access}-{i:02d}" for i in range(REFERENCES)]
    values = ["17" if access == "constant" else name for name in names]
    setup = "".join(f"(defvar pa-plain-{i:02d} 17)\n" for i in range(REFERENCES))
    setup += "".join(f"(defvaralias 'pa-alias-{i:02d} 'pa-plain-{i:02d})\n"
                     for i in range(REFERENCES))
    if mode == "bytecode":
        code = []
        for i in range(REFERENCES):
            if access == "constant":
                code.append(192 + i)
            elif i < 6:
                code.append(8 + i)
            else:
                code.extend((14, i))
            code.append(135 if i == REFERENCES - 1 else 136)
        read = "(fset 'pa-read (make-byte-code 0 (unibyte-string %s) [%s] 1))" % (
            " ".join(map(str, code)), " ".join(values))
    else:
        read = "(defun pa-read () %s)" % " ".join(values)
    return (";;; -*- lexical-binding: t -*-\n" + setup + read +
            "\n(dotimes (_ 10) (pa-read))\n(garbage-collect)\n"
            f"(dotimes (_ {n}) (pa-read))\n(princ \"ALIAS-BENCH-COMPLETE\\n\")\n(kill-emacs 0)\n")


def profile(job):
    label, binary, mode, access, n, pad = job
    name = f"{label}-{mode}-{access}-{n}-{pad}"
    source = OUT / "profiles" / f"{name}.el"
    source.parent.mkdir(exist_ok=True)
    source.write_text(program(mode, access, n))
    trace = source.with_suffix(".callgrind")
    identity = {**profile_artifacts(binary), "source_sha256": sha(source),
                "pad": pad, "mode": mode}
    saved = source.with_suffix(".json")
    if saved.exists():
        cached = json.loads(saved.read_text())
        if cached["identity"] == identity and cached["trace_sha256"] == sha(trace):
            return job, cached["ir"], cached["seconds"]
    env = dict(os.environ, ABI_LAYOUT_PAD="x" * pad,
               XDG_CACHE_HOME=str(OUT / "profiles" / f"cache-{name}-{time.time_ns()}"))
    if mode != "vm":
        env["NELISP_FX_DISABLE"] = "1"
    else:
        env.pop("NELISP_FX_DISABLE", None)
    start = time.monotonic()
    p = run(["valgrind", "--tool=callgrind", f"--callgrind-out-file={trace}",
             binary, "--load", source], env=env)
    source.with_suffix(".stdout").write_bytes(p.stdout)
    source.with_suffix(".stderr").write_bytes(p.stderr)
    assert p.returncode == 0 and p.stdout == b"ALIAS-BENCH-COMPLETE\n", (name, p.returncode, p.stdout[:120])
    total = next(int(line.split()[1]) for line in trace.open() if line.startswith("summary:"))
    assert total > 0
    elapsed = time.monotonic() - start
    saved.write_text(json.dumps({"identity": identity, "trace_sha256": sha(trace), "ir": total, "seconds": elapsed}))
    return job, total, elapsed


@functools.lru_cache(maxsize=4)
def symbol_table(binary, digest):
    symbols = []
    for line in subprocess.check_output(["nm", "-n", binary], text=True).splitlines():
        p = line.split()
        if len(p) == 3 and p[1] in ("t", "T"):
            symbols.append((int(p[0], 16), p[2]))
    return symbols, [a for a, _ in symbols]


def load_counts(path, binary):
    symbols, addresses = symbol_table(str(binary), sha(binary))
    selected = {"nelisp_env_lookup_value_w", "bf_dynamic_lookup_w", "wf_bytecode_w_varref",
                "nelisp_mirror_lookup_entry", "nl_pa_alias_read_w",
                "nelisp_env_lkv_mirror_with_frames_w", "nl_gc_sweep", "nl_sb_plain_entry_p",
                "m5_fx_alias_ref", "nelisp_env_alias_slot_candidate"}
    # varref is inlined in the baseline bytecode executor. Its original
    # checkpoint remains one call per varref; terminal alias checks have a
    # different caller and must not be counted as additional variable reads.
    edges = {f"{caller}->nl_sb_plain_entry_p"
             for caller in ("wf_bytecode_execute_frame", "wf_bytecode_w_varref")}
    # The general inliner can absorb the VM varref into these callers.
    # Count its remaining entry-classification call, once per read, rather
    # than requiring a function boundary that optimized code no longer has.
    edges.update(f"{caller}->nl_sb_plain_entry_p"
                 for caller in ("m5_fx_globals_word", "m5_fx_run_body"))
    # Bytecode can retain the shared slot-4 result directly, eliminating
    # the plain wrapper. Only source-read callers count, never the alias
    # helper's terminal classification.
    edges.update(f"{caller}->nelisp_env_alias_slot_candidate"
                 for caller in ("wf_bytecode_execute_frame", "wf_bytecode_w_varref",
                                "m5_fx_globals_word", "m5_fx_run_body"))
    counts = dict.fromkeys(selected, 0)
    counts.update(dict.fromkeys(edges, 0))
    names, target, caller = {}, None, None
    for line in path.read_text().splitlines():
        if line.startswith(("fn=", "cfn=")):
            token = line.split("=", 1)[1].strip()
            m = re.fullmatch(r"\((\d+)\)(?:\s+(.*))?", token)
            name = m[2] if m else token
            if name and name.startswith("0x"):
                index = bisect.bisect_right(addresses, int(name.split("'")[0], 16)) - 1
                name = symbols[index][1] if index >= 0 else name
            if m:
                if name:
                    names[m[1]] = name
                name = names.get(m[1])
            resolved = name.split("'", 1)[0] if name else None
            if line.startswith("fn="):
                caller, target = resolved, None
            else:
                target = resolved
        elif line.startswith("calls=") and target in selected:
            calls = int(line[6:].split()[0])
            counts[target] += calls
            edge = f"{caller}->{target}"
            if edge in edges:
                counts[edge] += calls
    return counts


def read_witness(mode, counts):
    if mode == "interpreter":
        return counts["nelisp_env_lookup_value_w"]
    if mode == "vm":
        # Uniform alias workloads can retain the guarded opcode's helper
        # instead of inlining it. Count that boundary once, not its callees.
        if counts["m5_fx_alias_ref"]:
            return counts["m5_fx_alias_ref"]
        if counts["wf_bytecode_w_varref"]:
            return counts["wf_bytecode_w_varref"]
        return counts["nelisp_env_lkv_mirror_with_frames_w"] + sum(
            counts[f"{caller}->{checkpoint}"]
            for caller in ("m5_fx_globals_word", "m5_fx_run_body")
            for checkpoint in ("nl_sb_plain_entry_p", "nelisp_env_alias_slot_candidate"))
    if counts["wf_bytecode_w_varref"]:
        return counts["wf_bytecode_w_varref"]
    return sum(counts[f"{caller}->{checkpoint}"]
               for caller in ("wf_bytecode_execute_frame", "wf_bytecode_w_varref")
               for checkpoint in ("nl_sb_plain_entry_p", "nelisp_env_alias_slot_candidate"))


def record_cost():
    for binary in (BASE, BIN):
        profile_artifacts(binary)
    jobs = [(label, binary, mode, access, n, pad)
            for label, binary in (("base", BASE), ("candidate", BIN))
            for mode in ("interpreter", "vm", "bytecode")
            for access in ("plain", "alias", "constant") for n in (100, 2100) for pad in PADS]
    with concurrent.futures.ThreadPoolExecutor(8) as pool:
        results = list(pool.map(profile, jobs))
    totals = {(j[0], j[2], j[3], j[4], j[5]): ir for j, ir, _ in results}
    data = {"base_sha256": sha(BASE), "candidate_sha256": sha(BIN),
            "base_cold_sha256": sha(str(BASE) + ".cold"),
            "candidate_cold_sha256": sha(str(BIN) + ".cold"),
            "harness_sha256": sha(__file__), "profile_evidence": {},
            "references_per_call": REFERENCES, "modes": {}}
    for job, _, _ in results:
        label, _, mode, access, n, pad = job
        stem = OUT / "profiles" / f"{label}-{mode}-{access}-{n}-{pad}"
        for suffix in (".el", ".json", ".callgrind"):
            path = stem.with_suffix(suffix)
            data["profile_evidence"][str(path.relative_to(ROOT))] = sha(path)
    counts = {(j[0], j[2], j[3], j[4], j[5]): load_counts(
        OUT / "profiles" / f"{j[0]}-{j[2]}-{j[3]}-{j[4]}-{j[5]}.callgrind", j[1]) for j, _, _ in results}
    for mode in ("interpreter", "vm", "bytecode"):
        row = {}
        for label in ("base", "candidate"):
            for access in ("plain", "alias", "constant"):
                values = [(totals[label, mode, access, 2100, pad] -
                           totals[label, mode, access, 100, pad]) / (2000 * REFERENCES) for pad in PADS]
                assert all(v > 0 for v in values)
                row[f"{label}_{access}"] = sum(values) / 4
                row[f"{label}_{access}_layouts"] = values
            for access in ("plain", "alias"):
                row[f"{label}_{access}_reference"] = row[f"{label}_{access}"] - row[f"{label}_constant"]
                assert row[f"{label}_{access}_reference"] > 0
                observed = []
                for pad in PADS:
                    deltas = {name: (counts[label, mode, access, 2100, pad][name] -
                                     counts[label, mode, access, 100, pad][name] -
                                     counts[label, mode, "constant", 2100, pad][name] +
                                     counts[label, mode, "constant", 100, pad][name]) / 2000
                              for name in counts[label, mode, access, 100, pad] if name != "nl_gc_sweep"}
                    assert read_witness(mode, deltas) == REFERENCES, (label, mode, access, pad, deltas)
                    observed.append(deltas)
                row[f"{label}_{access}_observed_loads"] = observed
        data["modes"][mode] = row
    (OUT / "cost.json").write_text(json.dumps(data, indent=2) + "\n")
    for mode in data["modes"]:
        check_cost(mode)


def check_cost(mode):
    data = json.loads((OUT / "cost.json").read_text())
    assert data["base_sha256"] == sha(BASE) and data["candidate_sha256"] == sha(BIN)
    assert data["base_cold_sha256"] == sha(str(BASE) + ".cold")
    assert data["candidate_cold_sha256"] == sha(str(BIN) + ".cold")
    assert data["harness_sha256"] == sha(__file__), "benchmark harness changed"
    evidence = {p: digest for p, digest in data["profile_evidence"].items()
                if f"-{mode}-" in Path(p).name}
    assert len(evidence) == 2 * 3 * 2 * len(PADS) * 3
    for path, digest in evidence.items():
        assert sha(ROOT / path) == digest, path
    assert data["references_per_call"] == REFERENCES
    row = data["modes"][mode]
    for label in ("base", "candidate"):
        for access in ("plain", "alias"):
            observed = row[f"{label}_{access}_observed_loads"]
            assert len(observed) == len(PADS)
            assert all(read_witness(mode, counts) == REFERENCES for counts in observed)
    ratio = assert_cost_bounds(row)
    print(mode, json.dumps(row), "ratio", ratio)


def assert_cost_bounds(row):
    ratio = row["candidate_alias_reference"] / row["candidate_plain_reference"]
    assert ratio <= 2 and row["candidate_plain_reference"] <= row["base_plain_reference"]
    return ratio


def record_load():
    spec = importlib.util.spec_from_file_location("abi_ir", ROOT / "tools/ai/abi-ir.py")
    abi = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(abi)
    source = ROOT / "test/perf/prim-speed-load-five.el"
    directory = OUT / "load-five"
    directory.mkdir(exist_ok=True)

    def one(job):
        label, binary, pad = job
        stem = directory / f"{label}-{pad}"
        cache = directory / f"cache-{label}-{pad}-{time.time_ns()}"
        cache.mkdir()
        env = dict(os.environ, ABI_LAYOUT_PAD="x" * pad, XDG_CACHE_HOME=str(cache))
        with stem.with_suffix(".stdout").open("wb") as out, stem.with_suffix(".stderr").open("wb") as err:
            p = subprocess.run(["valgrind", "--tool=callgrind", "--error-exitcode=97",
                                f"--callgrind-out-file={stem}.callgrind", str(binary), "--load", str(source)],
                               cwd=ROOT, env=env, stdout=out, stderr=err, timeout=580)
        assert p.returncode == 0, (label, pad, p.returncode)
        stdout = stem.with_suffix(".stdout").read_bytes()
        assert stdout == b"FIVE-LIBRARIES PASS: 5 loaded\n"
        ir, gc = abi.profile_counts(stem.with_suffix(".callgrind"), binary)
        return {"label": label, "pad": pad, "ir": ir, "gc": gc}

    jobs = [(label, binary, pad) for label, binary in (("base", BASE), ("candidate", BIN)) for pad in PADS]
    with concurrent.futures.ThreadPoolExecutor(8) as pool:
        rows = list(pool.map(one, jobs))
    evidence = {str(path.relative_to(ROOT)): sha(path)
                for path in directory.iterdir()
                if path.is_file() and path.suffix in (".callgrind", ".stdout", ".stderr")}
    data = {"base_sha256": sha(BASE), "candidate_sha256": sha(BIN),
            "base_cold_sha256": sha(str(BASE) + ".cold"),
            "candidate_cold_sha256": sha(str(BIN) + ".cold"),
            "harness_sha256": sha(__file__),
            "parser_sha256": sha(ROOT / "tools/ai/abi-ir.py"),
            "source_sha256": sha(source), "rows": rows, "evidence": evidence,
            "libraries": {path: sha(ROOT / path) for path in FIVE_FILES}}
    for label in ("base", "candidate"):
        data[label + "_mean"] = sum(r["ir"] for r in rows if r["label"] == label) / 4
    (OUT / "load-five.json").write_text(json.dumps(data, indent=2) + "\n")
    check_load()


def check_load():
    data = json.loads((OUT / "load-five.json").read_text())
    assert data["base_sha256"] == sha(BASE) and data["candidate_sha256"] == sha(BIN)
    assert data["base_cold_sha256"] == sha(str(BASE) + ".cold")
    assert data["candidate_cold_sha256"] == sha(str(BIN) + ".cold")
    assert data["harness_sha256"] == sha(__file__)
    assert data["parser_sha256"] == sha(ROOT / "tools/ai/abi-ir.py")
    assert data["source_sha256"] == sha(ROOT / "test/perf/prim-speed-load-five.el")
    assert set(data["libraries"]) == set(FIVE_FILES)
    for path, digest in data["libraries"].items():
        assert sha(ROOT / path) == digest, path
    assert len(data["evidence"]) == 2 * len(PADS) * 3
    for path, digest in data["evidence"].items():
        assert sha(ROOT / path) == digest, path
    for label in ("base", "candidate"):
        rows = [r for r in data["rows"] if r["label"] == label]
        assert sorted(r["pad"] for r in rows) == list(PADS)
        assert all(r["ir"] > 0 and r["gc"] >= 0 for r in rows)
        assert data[label + "_mean"] == sum(r["ir"] for r in rows) / 4
    print("ALIAS-FIVE", json.dumps(data))
    assert data["candidate_mean"] <= data["base_mean"]


def record_us():
    data = {"base_sha256": sha(BASE), "candidate_sha256": sha(BIN),
            "base_cold_sha256": sha(str(BASE) + ".cold"),
            "candidate_cold_sha256": sha(str(BIN) + ".cold"), "modes": {}}
    for mode in ("interpreter", "vm", "bytecode"):
        row = {}
        for label, binary in (("base", BASE), ("candidate", BIN)):
            for access in ("plain", "alias", "constant"):
                n = 20000
                source = OUT / f"time-{label}-{mode}-{access}.el"
                loop = f"(dotimes (_ {n}) (pa-read))"
                timed = ('(let ((start (float-time))) ' + loop +
                         f' (princ (format "ALIAS-US %.9f\\n" (* 1000000.0 (/ (- (float-time) start) {n * REFERENCES})))) )')
                source.write_text(program(mode, access, n).replace(loop, timed))
                env = dict(os.environ)
                if mode != "vm":
                    env["NELISP_FX_DISABLE"] = "1"
                else:
                    env.pop("NELISP_FX_DISABLE", None)
                p = run([binary, "--load", source], env=env)
                source.with_suffix(".stdout").write_bytes(p.stdout)
                source.with_suffix(".stderr").write_bytes(p.stderr)
                assert p.returncode == 0 and not p.stderr
                lines = p.stdout.decode().splitlines()
                assert len(lines) == 2 and lines[0].startswith("ALIAS-US ") and lines[1] == "ALIAS-BENCH-COMPLETE"
                row[f"{label}_{access}"] = float(lines[0].split()[1])
        data["modes"][mode] = row
    (OUT / "us.json").write_text(json.dumps(data, indent=2) + "\n")
    print(json.dumps(data))


def check_census():
    spec = importlib.util.spec_from_file_location("census", ROOT / "tools/ai/box-census.py")
    census = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(census)
    totals = {}
    for label in ("base", "candidate"):
        rows = census.load(OUT / label / "census.tsv")
        summary, _ = census.tally(rows)
        assert summary["units"] > 0
        assert all(any(k == census.CONVERSION_WALK_VERSION for _, k, _ in v) for v in rows.values())
        debt = sum(summary["idioms"].get(k, 0) for k in (*census.CONVERSION_CALLS, "inline-box-to-word"))
        assert debt == 0, (label, debt)
        totals[label] = sum(summary["by_unit"].values())
    print("ALIAS-CENSUS", json.dumps(totals), "S6.2=0")
    assert totals["candidate"] <= totals["base"]


def receipt(name):
    data = json.loads((OUT / f"{name}.json").read_text())
    assert data["candidate_sha256"] == sha(BIN) and data["pass"] is True
    assert data["candidate_cold_sha256"] == sha(str(BIN) + ".cold")
    for path, digest in data["evidence"].items():
        assert sha(ROOT / path) == digest, path
    print(f"ALIAS-{name.upper()} PASS")


def save_receipt(name, files, **extra):
    data = {"candidate_sha256": sha(BIN), "pass": True,
            "candidate_cold_sha256": sha(str(BIN) + ".cold"),
            "evidence": {str(p.relative_to(ROOT)): sha(p) for p in files}, **extra}
    (OUT / f"{name}.json").write_text(json.dumps(data, indent=2) + "\n")


def record_final():
    static = OUT / "static/nelisp"
    log = OUT / "final-verify.log"
    env = dict(os.environ, STANDALONE_CHECK_LOGDIR=str(OUT / "standalone-cases"))
    with log.open("wb") as out:
        p = subprocess.run(["bash", "target/final-verify/checks.sh", str(BIN), str(static), "prim-alias"],
                           cwd=ROOT, env=env, stdout=out, stderr=subprocess.STDOUT, timeout=7200)
    assert p.returncode == 0, log
    shadow = OUT / "shadow-smoke.log"
    with shadow.open("wb") as out:
        p = subprocess.run(["make", "-o", "standalone-reader", "standalone-reader-shadow-smoke", f"STANDALONE_BIN={BIN}"],
                           cwd=ROOT, env=env, stdout=out, stderr=subprocess.STDOUT, timeout=600)
    assert p.returncode == 0 and "[shadow-smoke] PASS:" in shadow.read_text()
    files = [log, shadow, *sorted((ROOT / "target/final-verify/checks-prim-alias").glob("*.log"))]
    save_receipt("final", files, static_sha256=sha(static),
                 static_cold_sha256=sha(str(static) + ".cold"))
    case_files = sorted((OUT / "standalone-cases").glob("*"))
    save_receipt("standalone", files + case_files)
    check_final()
    check_standalone()


def check_final():
    receipt("final")
    data = json.loads((OUT / "final.json").read_text())
    assert data["static_sha256"] == sha(OUT / "static/nelisp")
    assert data["static_cold_sha256"] == sha(OUT / "static/nelisp.cold")
    log = (OUT / "final-verify.log").read_text()
    rows = re.findall(r"^([\w-]+) rc=(\d+) \d+s$", log, re.M)
    assert len(rows) == 11 and all(rc == "0" for _, rc in rows)
    assert "GATE-COUNT: 11 cases, 0 failures" in log
    assert "[shadow-smoke] PASS:" in (OUT / "shadow-smoke.log").read_text()


def check_standalone():
    receipt("standalone")
    log = (ROOT / "target/final-verify/checks-prim-alias/standalone-checks.log").read_text()
    assert "STANDALONE-CHECKS PASS" in log and "STANDALONE-CHECK FAIL " not in log
    count = EXTRA.read_text().count("(pa-case '")
    assert f"VARIABLE-ALIAS PASS checked={count}\n" in (OUT / "standalone-cases/standalone-variable-alias-check.out").read_text()
    assert not (OUT / "standalone-cases/standalone-variable-alias-check.err").read_bytes()


def record_mcp():
    launcher = Path.home() / ".emacs.d/external-packages/anvil.el/tools/anvil-standalone-mcp.sh"
    stdout, stderr = OUT / "mcp.stdout", OUT / "mcp.stderr"
    requests = [
        {"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {
            "protocolVersion": "2024-11-05", "capabilities": {},
            "clientInfo": {"name": "prim-alias-smoke", "version": "1"}}},
        {"jsonrpc": "2.0", "method": "notifications/initialized"},
        {"jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {}}]
    with stdout.open("wb") as out, stderr.open("wb") as err:
        p = subprocess.Popen([str(launcher)], cwd=ROOT, env=dict(os.environ, NELISP_BIN=str(BIN)),
                             stdin=subprocess.PIPE, stdout=out, stderr=err)
        try:
            for request in requests[:2]:
                p.stdin.write((json.dumps(request) + "\n").encode())
            p.stdin.flush()
            start = time.monotonic()
            time.sleep(25)
            delay = time.monotonic() - start
            p.stdin.write((json.dumps(requests[2]) + "\n").encode())
            p.stdin.close()
            assert p.wait(timeout=180) == 0
        finally:
            if p.poll() is None:
                p.kill()
                p.wait()
    replies = [json.loads(line) for line in stdout.read_text().splitlines() if line.startswith("{")]
    init = next(row for row in replies if row.get("id") == 1)
    tools = next(row for row in replies if row.get("id") == 2)["result"]["tools"]
    assert "result" in init and len(tools) == len({t["name"] for t in tools}) == 40
    assert not stderr.read_bytes(), stderr
    save_receipt("mcp", [stdout, stderr], delay_seconds=delay, tools=len(tools))
    check_mcp()


def check_mcp():
    receipt("mcp")
    data = json.loads((OUT / "mcp.json").read_text())
    assert data["delay_seconds"] >= 25 and data["tools"] == 40


def check_delivery():
    commits = sorted((ROOT / "target/codex/commit-done").glob("*.sha"))
    assert commits, "coordinator has not committed the lane"
    commit = commits[-1].read_text().strip().split()[0]
    assert run(["git", "rev-parse", "HEAD"]).stdout.decode().strip() == commit
    assert not run(["git", "status", "--porcelain"]).stdout
    assert (OUT / "design.md").is_file()
    check_final()
    print("ALIAS-DELIVERY PASS", commit, sha(BIN), sha(str(BIN) + ".cold"))


def main():
    OUT.mkdir(exist_ok=True)
    action = sys.argv[1]
    if action == "parity":
        parity()
    elif action == "coverage":
        text = EXTRA.read_text()
        for family in ("reference-setq", "let-alias", "let-base", "re-alias", "cycle-data",
                       "nil-t-keyword", "constant-write-errors", "uninterned-nil", "long-chain",
                       "documentation", "delete-nonalias", "buffer-local", "multi-hop-operations",
                       "void-default", "old-lexical-base", "re-alias-local-base"):
            assert "'" + family in text, family
        print("ALIAS-COVERAGE PASS")
    elif action == "record-cost":
        record_cost()
    elif action == "record-load":
        record_load()
    elif action == "record-us":
        record_us()
    elif action == "record-final":
        record_final()
    elif action == "record-mcp":
        record_mcp()
    elif action == "census":
        check_census()
    elif action == "load-five":
        check_load()
    elif action == "cost":
        check_cost(sys.argv[2])
    elif action == "final":
        check_final()
    elif action == "standalone":
        check_standalone()
    elif action == "mcp":
        check_mcp()
    elif action == "delivery":
        check_delivery()
    else:
        receipt(action)


if __name__ == "__main__":
    main()
