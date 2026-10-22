#!/usr/bin/env python3
"""Focused acceptance entry points for Doc 227; unfinished checks fail closed."""
from pathlib import Path
import argparse
import json
import re
import os
import subprocess
import time
import hashlib
import select
import signal
import struct
import resource
import tempfile

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "target/strings"
CENSUS = {
    "transport": ["nl_word_clone", "wf_word_view", "nl_sexp_clone_into", "nl_g4arm_copy_w"],
    "alias": ["nl_sci_copy", "nl_sci_dispatch"],
    "allocation": ["nl_alloc_str_w", "nl_alloc_unibyte_str_w", "nelisp_sexp_write_str"],
    "copy": ["nl_alloc_str_copy_loop", "nl_mut_str_finalize_copy_loop", "nl_mut_str_finalize"],
    "read": ["nl_str_bytes_ptr", "nl_str_char_count"],
    "publication": ["m5_finish_string_w"],
    "retag": ["nelisp_reader_p_build_head_symbol"],
    "name_owner": ["nelisp_mirror_bucket_prepend_unchecked"],
}


def plan():
    doc = (ROOT / "docs/design/227-gnu-string-port.org").read_text()
    rows = []
    for path in sorted((ROOT / "lisp").glob("*.el")) + [ROOT / "scripts/nelisp-standalone-build.el"]:
        source = path.read_text()
        for family, names in CENSUS.items():
            for name in names:
                for match in re.finditer(r"\(defun\s+" + re.escape(name) + r"(?=\s|\()", source):
                    rows.append(dict(family=family, name=name, path=str(path.relative_to(ROOT)),
                                     line=source.count("\n", 0, match.start()) + 1))
    assert len(rows) == 17, f"census changed: {len(rows)} definitions; classify before accepting"
    for token in ["alloc.c:1729", "character.h:", "fns.c:", "nl_sweep_strings",
                  "nl_gc_mark_reset_all", "nl_gc_conserv_typed", "nl_dump_compact_write"]:
        assert token in doc, f"missing plan mechanism: {token}"
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "census.json").write_text(json.dumps(rows, indent=2) + "\n")
    print("PASS plan: GNU mechanism map, 17 classified definitions, GC integration contract")


def kernel():
    env = os.environ.copy()
    env["TMPDIR"] = str(ROOT / "target/tmp")
    commands = [
        ["emacs", "-Q", "--batch", "-L", "lisp", "-L", "src", "-L", "scripts",
         "--eval", "(setq backtrace-on-error-noninteractive nil)",
         "-l", "tools/ai/strings-kernel.el"],
        ["gcc", "-O2", "-no-pie", "-pthread", "tools/ai/strings-kernel.c",
         "target/strings/kernel.o", "-o", "target/strings/kernel"],
        [str(OUT / "kernel")],
    ]
    start = time.monotonic()
    for i, command in enumerate(commands):
        result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True, timeout=30)
        (OUT / f"kernel-step-{i}.out").write_bytes(result.stdout)
        (OUT / f"kernel-step-{i}.err").write_bytes(result.stderr)
        assert result.returncode == 0, f"kernel step {i} failed; see target/strings/kernel-step-{i}.err"
    assert result.stdout.startswith(b"PASS string kernel:"), "missing completion assertion"
    assert not result.stderr, "unexpected kernel stderr"
    (OUT / "kernel-result.json").write_text(json.dumps(dict(
        status="PASS", header_bytes=32, sdata_bytes=24, string8_bytes=56,
        wall_seconds=time.monotonic()-start)) + "\n")
    print(result.stdout.decode().strip())


def semantics():
    fixture = "tools/ai/strings-semantics.el"
    outputs = {}
    runtimes = [("gnu", [os.environ.get("GNU_EMACS", "emacs"), "-Q", "--batch"]),
                ("dynamic", [os.environ.get("STRINGS_DYNAMIC", str(OUT / "dynamic/nelisp"))]),
                ("static", [os.environ.get("STRINGS_STATIC", str(OUT / "static/nelisp"))])]
    for kind, command in runtimes:
        assert Path(command[0]).exists() or kind == "gnu", f"missing {kind} candidate"
        result = subprocess.run(command + ["--load", fixture], cwd=ROOT, capture_output=True, timeout=40)
        (OUT / f"{kind}-semantics.out").write_bytes(result.stdout)
        (OUT / f"{kind}-semantics.err").write_bytes(result.stderr)
        assert result.returncode == 0, f"{kind} fixture exit {result.returncode}"
        assert not result.stderr, f"{kind} unexpected stderr"
        outputs[kind] = [line for line in result.stdout.splitlines() if line.startswith(b"STRINGS-CASE ")]
        assert len(outputs[kind]) >= 10, f"{kind} incomplete fixture"
    assert outputs["dynamic"] == outputs["gnu"], "dynamic GNU fixture differs; see saved outputs"
    assert outputs["static"] == outputs["gnu"], "static GNU fixture differs; see saved outputs"
    # GNU fns.c:951-1021 copies intervals only for a string that has them: the
    # marker-guarded concat/substring paths, second generation included.
    props = {}
    for kind, command in runtimes:
        result = subprocess.run(command + ["--load", "tools/ai/strings-props.el"],
                                cwd=ROOT, capture_output=True, timeout=40)
        (OUT / f"{kind}-props.out").write_bytes(result.stdout)
        (OUT / f"{kind}-props.err").write_bytes(result.stderr)
        assert result.returncode == 0 and not result.stderr, f"{kind} property fixture failed"
        props[kind] = [line for line in result.stdout.splitlines() if line.startswith(b"STRINGS-PROP ")]
        assert len(props[kind]) == 8, f"{kind} incomplete property fixture"
    assert props["dynamic"] == props["gnu"], "dynamic GNU property fixture differs; see saved outputs"
    assert props["static"] == props["gnu"], "static GNU property fixture differs; see saved outputs"
    # Reuse the existing byte-mutation oracle: final checks exposed a
    # multibyte symbol-name boundary regression absent from the short fixture.
    matrix = {}
    for kind, command in runtimes:
        result = subprocess.run(command + ["--load", "test/standalone-unibyte-aset-check.el"],
                                cwd=ROOT, capture_output=True, timeout=40)
        (OUT / f"{kind}-unibyte-matrix.out").write_bytes(result.stdout)
        (OUT / f"{kind}-unibyte-matrix.err").write_bytes(result.stderr)
        assert result.returncode == 0 and not result.stderr, f"{kind} unibyte matrix failed"
        matrix[kind] = result.stdout
    assert matrix["dynamic"] == matrix["gnu"], "dynamic GNU unibyte/symbol-name matrix differs"
    assert matrix["static"] == matrix["gnu"], "static GNU unibyte/symbol-name matrix differs"
    print(f"PASS semantics: {len(outputs['gnu'])} GNU cases, {len(props['gnu'])} property cases and existing unibyte matrix on dynamic/static")


def root_layout(binary, symbols, n, mode):
    """Inspect this check's own paused child; never publish raw addresses."""
    env = dict(os.environ, STRINGS_N=str(n), STRINGS_MODE=mode)
    fixture = ROOT / "tools/ai/strings-layout.el"
    form = "(load " + json.dumps(str(fixture)) + ")"
    child = subprocess.Popen([str(binary), "--eval", form], cwd=ROOT, env=env,
                             stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE, bufsize=0)
    output = b""
    try:
        deadline = time.monotonic() + 60
        while b"STRINGS-HOLD\n" not in output and time.monotonic() < deadline:
            ready, _, _ = select.select([child.stdout], [], [], 1)
            if ready:
                block = os.read(child.stdout.fileno(), 4096)
                if not block:
                    break
                output += block
        assert b"STRINGS-HOLD\n" in output, "layout child did not reach retained-root barrier"
        child.send_signal(signal.SIGSTOP)
        _, stopped = os.waitpid(child.pid, os.WUNTRACED)
        assert os.WIFSTOPPED(stopped), "layout child exited before inspection"
        with open(f"/proc/{child.pid}/mem", "rb", buffering=0) as memory:
            def raw(address, count):
                return os.pread(memory.fileno(), count, address)
            def u(address):
                return struct.unpack("<Q", raw(address, 8))[0]
            def payload(word):
                # A vectorlike WORD (110) and a direct cons WORD (010) address their
                # payload at the word minus the tag; a legacy slot holds a box pointer.
                if word & 7 == 6:
                    return word - 6
                return word - 2 if word & 7 == 2 else u(word + 8)
            def slot(word, index):
                return u(payload(word) + 16 + index * 8)
            environment = u(symbols["nl_gc_loop_ctx"] + 64)
            table = slot(environment, 0)
            capacity, buckets = slot(table, 0) >> 2, slot(table, 1)
            assert 0 < capacity <= 65536, "invalid environment bucket count"
            value = None
            for index in range(capacity):
                link, steps = u(payload(buckets) + 8 + index * 8), 0
                while link > 4096 and steps < 10000:
                    pair = u(payload(link))
                    key, entry = u(payload(pair)), u(payload(pair) + 8)
                    length = u(key - 4 + 8) if key & 7 == 4 else u(key + 24)
                    if key & 7 == 4 and length == 2**64 - 1:
                        length = u(key - 4) & (2**63 - 1)
                    if length == len("strings-layout-keep"):
                        data = u(key - 4 + 24) if key & 7 == 4 else u(key + 16)
                        if raw(data, length) == b"strings-layout-keep":
                            value = slot(entry, 0)
                            break
                    link, steps = u(payload(link) + 8), steps + 1
                if value is not None:
                    break
            assert value is not None, "retained vector root not found"
            kind = u(value - 6) & 63 if value & 7 == 6 else raw(value, 1)[0]
            if kind == 11:
                value = u(payload(value))
            assert value & 7 == 6 and u(value - 6) & 63 == 8, "root is not a vector WORD"
            assert u(value - 6) >> 8 == n, "retained vector length differs"
            words = [u(value - 6 + 8 + i * 8) for i in range(n)]
            canonical = all(word & 7 == 4 for word in words)
            data_addresses, legacy_totals = [], []
            for word in words:
                if canonical:
                    header = word - 4
                    assert header % 32 == 0 and u((header & -32768) + 32752) == 3
                    assert u(header) == 8 and u(header + 8) == 2**64 - 1
                    data = u(header + 24)
                    assert u(data - 8) == header and raw(data, 9) == b"xxxxxxxx\0"
                else:
                    assert word & 7 == 0 and raw(word, 1)[0] in (5, 14)
                    data = u(word + 16)
                    assert u(word + 24) == 8 and raw(data, 8) == b"xxxxxxxx"
                    legacy_totals.append(((u(word - 8) & 0xFFFFFFF8), (u(data - 8) & 0xFFFFFFF8)))
                data_addresses.append(data)
            assert (len(set(data_addresses)) == 1) == (mode == "aliases")
            if canonical:
                assert len(set(words)) == (n if mode == "population" else 1)
                ordered = sorted(set(data_addresses))
                if len(ordered) > 1:
                    assert min(b - a for a, b in zip(ordered, ordered[1:])) == 24
                physical = 32 + 24
                alias_view = 0
            else:
                assert set(legacy_totals) == {(40, 16)}, "base allocation shape changed"
                physical, alias_view = 56, 40
            result = dict(canonical=canonical, roots=n, distinct_headers=len(set(words)),
                          distinct_data=len(set(data_addresses)), string8_bytes=physical,
                          extra_view_bytes_per_alias=alias_view)
        child.send_signal(signal.SIGCONT)
        child.stdin.write(b"nil\n")
        child.stdin.close()
        child.stdin = None
        tail, error = child.communicate(timeout=15)
        assert child.returncode == 0 and not error, "layout child failed after inspection"
        return result, output + tail
    finally:
        if child.poll() is None:
            child.kill()
            child.wait()


def layout():
    result = {"status": "PASS", "header_bytes": 32, "sdata_bytes": 24}
    for role in ["base", "dynamic"]:
        binary = OUT / role / "nelisp"
        assert binary.exists(), f"missing {role} binary"
        symbols = {}
        for line in subprocess.check_output(["nm", "--defined-only", str(binary)], text=True).splitlines():
            fields = line.split()
            if len(fields) == 3:
                symbols[fields[2]] = int(fields[0], 16)
        rows = {}
        for mode in ["population", "aliases"]:
            rows[mode] = []
            for n in [4000, 8000]:
                row, output = root_layout(binary, symbols, n, mode)
                (OUT / role / f"root-{mode}-{n}.out").write_bytes(output)
                rows[mode].append(row)
                assert row["canonical"] == (role == "dynamic"), "string representation differs"
        result[role] = dict(sha256=hashlib.sha256(binary.read_bytes()).hexdigest(), roots=rows)
    def current_layout(row):
        assert row["canonical"], "legacy String view remains"
        assert row["string8_bytes"] <= 56 and row["extra_view_bytes_per_alias"] == 0
    # Run the same acceptance predicate on the known pre-port shape.
    try:
        current_layout(result["base"]["roots"]["aliases"][0])
    except AssertionError:
        pass
    else:
        raise AssertionError("layout predicate accepted the legacy base")
    for mode in ["population", "aliases"]:
        for row in result["dynamic"]["roots"][mode]:
            current_layout(row)
    kernel_result = json.loads((OUT / "kernel-result.json").read_text())
    assert kernel_result["status"] == "PASS" and kernel_result["string8_bytes"] == 56
    result.update(before_bytes=56, after_bytes=56, before_alias_bytes=40, after_alias_bytes=0,
                  negative_control="legacy base rejected by canonical WORD predicate")
    (OUT / "layout.json").write_text(json.dumps(result, indent=2) + "\n")
    print("PASS layout: 56 -> 56 B/string; 40 -> 0 B/alias; retained roots are canonical WORDs")

def speed(workload):
    assert workload, "choose producers or pressure"
    report = json.loads(Path(os.environ.get("STRINGS_SPEED_REPORT", str(OUT / "timing" / f"{workload}.json"))).read_text())
    assert report["cpu"] == 14 and report["rounds"] == 7
    assert set(report["verdicts"]) == ({"concat", "substring", "format"}
                                       if workload == "producers" else {"pressure"}), "incomplete workload report"
    for role, path in [("candidate", Path(os.environ.get("STRINGS_SPEED_CANDIDATE", str(OUT / "dynamic/nelisp")))), ("base", Path(os.environ.get("STRINGS_SPEED_BASE", str(OUT / "base/nelisp"))))]:
        assert report["identities"][role] == hashlib.sha256(path.read_bytes()).hexdigest(), "stale timing binary"
    for kind, row in report["verdicts"].items():
        assert len(row["paired_gnu_ratios"]) == len(row["paired_base_ratios"]) == 7
        assert row["passed"] and (row["base_ratio"] < 1 if workload == "pressure" else row["gnu_ratio"] <= 1), f"speed target missed: {kind} {row}"
        print(f"PASS {kind}: GNU {row['gnu_ratio']:.4f}x; base {row['base_ratio']:.4f}x")


def image():
    """Cold image: the companion image starts the binary, and strings survive a dump and a reload."""
    def limit():
        resource.setrlimit(resource.RLIMIT_AS, (8 << 30, 8 << 30))
    summary = []
    for kind in ("dynamic", "static"):
        binary = Path(os.environ.get("STRINGS_" + kind.upper(), str(OUT / kind / "nelisp")))
        cold = Path(str(binary) + ".cold")
        assert cold.exists() and cold.stat().st_size > (1 << 20), f"{kind}: no companion cold image"
        times = []
        for _ in range(5):
            started = time.monotonic()
            result = subprocess.run([str(binary), "--eval", "(princ (+ 1 2))"], capture_output=True,
                                    timeout=60, preexec_fn=limit)
            times.append(time.monotonic() - started)
            assert result.stdout.startswith(b"3") and not result.stderr, f"{kind}: cold start output"
        startup = sorted(times)[2]
        # A prelude boot takes seconds; the companion image takes tens of milliseconds.
        assert startup < 1.0, f"{kind}: startup {startup:.3f} s, the cold image was not used"
        with tempfile.TemporaryDirectory(dir=OUT) as tmp:
            dumped = Path(tmp) / "strings.img"
            environment = dict(os.environ, STRINGS_IMAGE_OUT=str(dumped))
            first = subprocess.run([str(binary), "--load", "tools/ai/strings-image-a.el"], cwd=ROOT,
                                   env=environment, capture_output=True, timeout=120, preexec_fn=limit)
            assert dumped.exists() and dumped.stat().st_size > (1 << 20), \
                f"{kind}: the dump wrote no image: {first.stderr[-200:]!r}"
            second = subprocess.run([str(binary), "--cold-load-from", str(dumped), "--load",
                                     "tools/ai/strings-image-b.el"], cwd=ROOT, capture_output=True,
                                    timeout=120, preexec_fn=limit)
            (OUT / f"{kind}-image.out").write_bytes(second.stdout)
            (OUT / f"{kind}-image.err").write_bytes(second.stderr)
            lines = second.stdout.splitlines()
            bad = [line for line in lines if line.endswith(b" FAIL")]
            passed = [line for line in lines if line.endswith(b" ok")]
            assert second.returncode == 0 and not second.stderr, f"{kind}: reload exit {second.returncode}"
            assert not bad and b"STRINGS-IMAGE-TOTAL fails=0" in lines and len(passed) >= 36, \
                f"{kind}: image check {bad[:3]}"
            summary.append(f"{kind} {startup * 1000:.0f} ms, {len(passed)} checks, image {dumped.stat().st_size >> 20} MiB")
    print("PASS image: " + "; ".join(summary))


def final():
    report = json.loads((OUT / "final.json").read_text())
    assert report["groups"] == 11 and report["failures"] == 0
    assert report["standalone_checks"] and report["mcp_tools"] == 40
    assert report["wine_smoke"], "Wine smoke still needed for this revision"
    for role in ["dynamic", "static"]:
        assert report["identities"][role] == hashlib.sha256((OUT / report.get("paths", {}).get(role, role + "/nelisp")).read_bytes()).hexdigest(), "stale final binary"
    print("PASS final: 11 groups, standalone checks, 40 MCP tools, Wine smoke")


def format_semantics():
    outputs = {}
    golden = os.environ.get("GNU_EMACS", "emacs")
    version = subprocess.check_output([golden, "--version"], text=True).splitlines()[0]
    assert version == "GNU Emacs 31.1", "format golden must be GNU Emacs 31.1"
    runtimes = [("gnu", [golden, "-Q", "--batch"]),
                ("dynamic", [str(OUT / "format-dynamic/nelisp")]),
                ("static", [str(OUT / "format-static/nelisp")])]
    for role, command in runtimes:
        result = subprocess.run(command + ["--load", "tools/ai/strings-semantics.el"],
                                cwd=ROOT, capture_output=True, timeout=55)
        (OUT / f"format-{role}.out").write_bytes(result.stdout)
        (OUT / f"format-{role}.err").write_bytes(result.stderr)
        assert result.returncode == 0 and not result.stderr, f"{role} format oracle failed"
        outputs[role] = [line for line in result.stdout.splitlines() if line.startswith(b"STRINGS-CASE ")]
        assert len([line for line in outputs[role] if line.startswith(b"STRINGS-CASE fmt-")]) == 44
    assert len(outputs["gnu"]) == 57
    for role in ["dynamic", "static"]:
        assert outputs[role] == outputs["gnu"], f"{role} GNU oracle differs: see format-{role}.out"
    identities = {role: hashlib.sha256(Path(command[0]).read_bytes()).hexdigest()
                  for role, command in runtimes}
    for role, command in runtimes[1:]:
        identities[role + "-cold"] = hashlib.sha256(Path(command[0] + ".cold").read_bytes()).hexdigest()
    (OUT / "format-oracle.json").write_text(json.dumps(dict(identities=identities,
        fixture_sha256=hashlib.sha256((ROOT / "tools/ai/strings-semantics.el").read_bytes()).hexdigest(),
        cases=len(outputs["gnu"]), format_cases=sum(line.startswith(b"STRINGS-CASE fmt-") for line in outputs["gnu"]), passed=True), indent=2) + "\n")
    print(f"PASS GNU format oracle: {len(outputs['gnu'])} cases, both linkages")


def format_speed():
    report = json.loads((OUT / "format-timing/producers.json").read_text())
    assert report["harness_sha256"] == hashlib.sha256((ROOT / "tools/ai/strings-wall.py").read_bytes()).hexdigest(), "stale timing harness"
    assert report["fixture_sha256"] == hashlib.sha256((ROOT / "tools/ai/strings-timing.el").read_bytes()).hexdigest(), "stale timing fixture"
    assert report["cpu"] == 14 and report["rounds"] == 7
    assert set(report["verdicts"]) == {"concat", "substring", "format"}
    for role, path in [("candidate", OUT / "format-dynamic/nelisp"), ("base", OUT / "format-base/nelisp")]:
        assert report["identities"][role] == hashlib.sha256(path.read_bytes()).hexdigest(), "stale timing binary"
        assert report["identities"][role + "-cold"] == hashlib.sha256(Path(str(path) + ".cold").read_bytes()).hexdigest(), "stale timing image"
    for kind, row in report["verdicts"].items():
        assert len(row["paired_base_ratios"]) == len(row["paired_gnu_ratios"]) == 7
        assert row["base_metric"] == ("own cost" if kind == "format" else "total call")
        assert row["base_ratio"] < (1 if kind == "format" else 1.03), f"{kind} regression"
        print(f"PASS {kind}: {row['base_ratio']:.4f}x b1f0ed553; {row['gnu_ratio']:.4f}x GNU")
    ir = json.loads((OUT / "format-ir.json").read_text())
    assert ir["iterations"] > 0 and ir["candidate_own_ir"] < ir["base_own_ir"]
    assert ir["identities"]["candidate"] == report["identities"]["candidate"]
    assert ir["identities"]["base"] == report["identities"]["base"]
    print(f"PASS format own Ir: {ir['base_own_ir']:.1f} -> {ir['candidate_own_ir']:.1f}")


def format_final():
    report = json.loads((OUT / "format-final.json").read_text())
    oracle = json.loads((OUT / "format-oracle.json").read_text())
    assert report["groups"] == 11 and report["failures"] == 0
    assert report["standalone_checks"] and report["mcp_tools"] == 40
    for role in ["dynamic", "static"]:
        identity = hashlib.sha256((OUT / f"format-{role}/nelisp").read_bytes()).hexdigest()
        assert report["identities"][role] == oracle["identities"][role] == identity, "stale final binary"
        cold = hashlib.sha256((OUT / f"format-{role}/nelisp.cold").read_bytes()).hexdigest()
        assert report["identities"][role + "-cold"] == oracle["identities"][role + "-cold"] == cold, "stale final image"
    assert oracle["passed"]
    print("PASS format final: both linkages, GNU oracle, 11 groups, standalone checks, 40 MCP tools")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("check", choices=["plan", "layout", "compaction", "semantics", "speed", "image", "final", "format", "format-speed", "format-final"])
    parser.add_argument("workload", nargs="?", choices=["producers", "pressure"])
    args = parser.parse_args()
    OUT.mkdir(parents=True, exist_ok=True)
    if args.check == "plan":
        plan()
    elif args.check == "compaction":
        kernel()
    elif args.check == "semantics":
        semantics()
    elif args.check == "layout":
        layout()
    elif args.check == "speed":
        speed(args.workload)
    elif args.check == "image":
        image()
    elif args.check == "final":
        final()
    elif args.check == "format":
        format_semantics()
    elif args.check == "format-speed":
        format_speed()
    elif args.check == "format-final":
        format_final()


if __name__ == "__main__":
    main()
