#!/usr/bin/env python3
"""Pinned GNU 31.1 case-table parity and inexpensive final-evidence checks.

api/behaviour execute the fixture against BOTH deployed artifacts, without
loading implementation sources.  oracle refreshes the pinned GNU outputs;
base records the required negative control.  sync is an explicit maintainer
operation which copies the canonical Lisp implementation into the bootstrap
prelude.  final verifies completed heavy-job evidence, never reruns the suite.
efficiency measures 14 serial API-presence processes against one batch.

Paths are relative to the checkout unless overridden with CASETAB_* variables.
The default artifact directories are target/casetab/{dynamic,static,base}.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import signal
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
MODES = ("api", "behaviour")
BEGIN = ";; BEGIN GENERATED CASETAB SOURCES"
END = ";; END GENERATED CASETAB SOURCES"
SOURCES = (
    "packages/nelisp-emacs-foundation/src/emacs-cc-casetab-1.el",
    "packages/nelisp-emacs-foundation/src/case-table.el",
)
PRELUDE = "scripts/nelisp-stdlib-prelude.el"


def path(value):
    candidate = Path(value)
    return candidate if candidate.is_absolute() else ROOT / candidate


OUT = path(os.environ.get("CASETAB_EVIDENCE_DIR", "target/casetab"))


def relative(value):
    try:
        return str(Path(value).resolve().relative_to(ROOT))
    except ValueError:
        return str(Path(value).resolve())


def sha256(file):
    digest = hashlib.sha256()
    with Path(file).open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def write_json(file, value):
    file.parent.mkdir(parents=True, exist_ok=True)
    file.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n")


def fixture(mode):
    return ROOT / f"test/standalone-casetab-{mode}-check.el"


def expected(mode):
    return fixture(mode).with_suffix(".expected")


def generated_block():
    # Preserve the original Lisp literally, including its lexical-binding
    # header.  Headers in the middle of the prelude are harmless comments.
    return (BEGIN + "\n" +
            "\n".join(path(source).read_text().rstrip("\n")
                      for source in SOURCES) + "\n" + END)


def check_sync(update=False):
    file = path(PRELUDE)
    text = file.read_text()
    begin, end = text.find(BEGIN), text.find(END)
    if text.count(BEGIN) > 1 or text.count(END) > 1:
        raise ValueError("duplicate generated case-table source markers")
    block = generated_block()
    if begin < 0 and end < 0:
        if not update:
            raise ValueError("prelude lacks generated case-table block; run sync")
        text = text.rstrip("\n") + "\n\n" + block + "\n"
    elif begin < 0 or end < begin:
        raise ValueError("incomplete generated case-table source markers")
    elif text[begin:end + len(END)] != block:
        if not update:
            raise ValueError("generated case-table prelude drift; run sync")
        text = text[:begin] + block + text[end + len(END):]
    if update:
        file.write_text(text)


def run(command, directory, name, timeout):
    directory.mkdir(parents=True, exist_ok=True)
    started = time.monotonic()
    # The shell applies the same memory cap to every oracle and runtime test.
    # Arguments go through argv, never shell interpolation.
    capped = ["bash", "-c", 'ulimit -v 8388608; exec "$@"', "casetab"] + command
    process = subprocess.Popen(capped, cwd=ROOT, stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE, start_new_session=True)
    timed_out = False
    try:
        stdout, stderr = process.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        timed_out = True
        os.killpg(process.pid, signal.SIGKILL)
        stdout, stderr = process.communicate()
    out, err = directory / f"{name}.out", directory / f"{name}.err"
    out.write_bytes(stdout)
    err.write_bytes(stderr)
    result = {
        "command": command, "rc": process.returncode, "timeout": timed_out,
        "seconds": round(time.monotonic() - started, 6),
        "stdout": relative(out), "stdout_sha256": sha256(out),
        "stderr": relative(err), "stderr_sha256": sha256(err),
    }
    return result, stdout, stderr


def rows(data, mode):
    lines = data.decode("utf-8").splitlines()
    prefix = f"(CASETAB {mode} "
    actual = [line for line in lines if line.startswith(prefix)]
    marker = re.fullmatch(rf"CASETAB-DONE {mode} ([0-9]+)", lines[-1]) if lines else None
    if not marker or int(marker[1]) == 0 or int(marker[1]) != len(actual):
        raise ValueError(f"{mode}: missing/invalid completion marker or row count")
    if len(lines) != len(actual) + 1:
        raise ValueError(f"{mode}: unexpected output outside fixture rows")
    names = [re.match(r'^\(CASETAB \w+ "([^"]+)" ', row) for row in actual]
    if not all(names) or len({name[1] for name in names}) != len(actual):
        raise ValueError(f"{mode}: malformed or duplicate row names")
    return actual


def compare(actual, wanted, mode):
    expected_rows = rows(wanted, mode)
    try:
        actual_rows = rows(actual, mode)
        output_error = None
    except (UnicodeDecodeError, ValueError) as error:
        actual_rows = actual.decode("utf-8", errors="replace").splitlines()
        output_error = str(error)
    differences = []
    for index in range(max(len(expected_rows), len(actual_rows))):
        want = expected_rows[index] if index < len(expected_rows) else "<absent>"
        got = actual_rows[index] if index < len(actual_rows) else "<absent>"
        if want != got:
            differences.append({"row": index + 1, "expected": want, "actual": got})
    return {"count": len(expected_rows), "actual_count": len(actual_rows),
            "different_rows": len(differences), "first_difference":
            differences[0] if differences else None,
            "differences": differences, "output_error": output_error,
            "exact_stdout": actual == wanted}


def binary(kind):
    return path(os.environ.get(f"CASETAB_{kind.upper()}_BIN",
                               relative(OUT / kind / "nelisp")))


def identity(file):
    # The cold image and native startup are part of the deployed artifact.
    return {relative(candidate): sha256(candidate)
            for candidate in (file, Path(str(file) + ".cold"),
                              Path(str(file) + ".native-startup.el"))
            if candidate.is_file()}


def run_mode(mode, kind, timeout):
    executable = binary(kind)
    if not executable.is_file():
        raise ValueError(f"missing {kind} executable: {relative(executable)}")
    result, stdout, stderr = run([str(executable), "--load", relative(fixture(mode))],
                                 OUT / ("red" if kind == "base" else kind), mode, timeout)
    result.update(compare(stdout, expected(mode).read_bytes(), mode))
    result.update({"mode": mode, "artifact": kind, "binaries": identity(executable),
                   "fixture_sha256": sha256(fixture(mode)),
                   "expected_sha256": sha256(expected(mode))})
    result["pass"] = (result["rc"] == 0 and not result["timeout"] and
                      not stderr and result["exact_stdout"] and
                      result["output_error"] is None)
    # Elapsed time is diagnostic only; repeated successful meter runs must
    # leave the hash-bound parity evidence unchanged.
    proof = {key: value for key, value in result.items() if key != "seconds"}
    write_json(OUT / ("red" if kind == "base" else kind) / f"{mode}.json", proof)
    print(f"CASETAB {kind} {mode}: {result['count']} cases, "
          f"{result['different_rows']} differing rows, "
          f"{'PASS' if result['pass'] else 'FAIL'} ({result['seconds']:.3f}s)")
    if result["first_difference"]:
        first = result["first_difference"]
        print(f"  first row {first['row']}: expected {first['expected']}")
        print(f"  actual: {first['actual']}")
    if stderr:
        print(f"  unexpected stderr: {result['stderr']}")
    return result


def oracle(timeout):
    executable = os.environ.get("GNU_EMACS")
    if not executable:
        executable = next((shutil.which(name) for name in
                           ("emacs31.1", "emacs-31.1", "emacs")
                           if shutil.which(name)), None)
    if not executable:
        raise ValueError("GNU_EMACS or GNU Emacs 31.1 executable is required")
    executable = str(path(executable)) if "/" in executable else shutil.which(executable)
    results = []
    for mode in MODES:
        version_check = ('(progn (unless (string= emacs-version "31.1") '
                         '(error "Expected GNU Emacs 31.1")) '
                         '(require (quote case-table)))')
        result, stdout, stderr = run([executable, "-Q", "--batch", "--eval",
                                     version_check, "--load", relative(fixture(mode))],
                                     OUT / "gnu", mode, timeout)
        if result["rc"] != 0 or stderr or result["timeout"]:
            raise ValueError(f"GNU oracle {mode} failed; inspect {relative(OUT / 'gnu')}")
        result.update({"count": len(rows(stdout, mode)),
                       "fixture_sha256": sha256(fixture(mode)),
                       "oracle_sha256": sha256(executable)})
        expected(mode).write_bytes(stdout)
        results.append(result)
        print(f"CASETAB GNU {mode}: {result['count']} pinned cases")
    write_json(OUT / "gnu/manifest.json", {"version": "31.1", "runs": results})


def efficiency(timeout):
    """Measure real serial and batched API-presence subprocesses on the base."""
    declaration = re.search(r"\(dolist \(name '\((.*?)\)\)",
                            fixture("api").read_text(), re.S)
    if not declaration:
        raise ValueError("cannot find the pinned fixture's API presence list")
    names = declaration[1].split()
    if len(names) != 14 or len(set(names)) != 14:
        raise ValueError("efficiency comparison requires 14 distinct API queries")
    executable = binary("base")
    if not executable.is_file():
        raise ValueError("copied base executable is required for efficiency")
    serial_runs, values = [], []
    started = time.monotonic()
    for index, name in enumerate(names, 1):
        expression = f"(list (quote {name}) (fboundp (quote {name})))"
        result, stdout, stderr = run([str(executable), "--eval", expression],
                                     OUT / "efficiency", f"serial-{index:02d}", timeout)
        if result["rc"] != 0 or result["timeout"] or stderr or not stdout.strip():
            raise ValueError(f"serial efficiency query failed: {name}")
        serial_runs.append(result)
        values.append(stdout.strip())
    serial_seconds = time.monotonic() - started
    expression = ("(mapcar (lambda (name) (list name (fboundp name))) "
                  "(quote (" + " ".join(names) + ")))")
    started = time.monotonic()
    batch, stdout, stderr = run([str(executable), "--eval", expression],
                                OUT / "efficiency", "batched", timeout)
    batch_seconds = time.monotonic() - started
    combined = b"(" + b" ".join(values) + b")\n"
    exact = stdout == combined
    report = {
        "queries": names, "query_count": len(names), "binaries": identity(executable),
        "serial": {"process_count": len(serial_runs),
                   "seconds": round(serial_seconds, 6), "runs": serial_runs},
        "batched": {"process_count": 1, "seconds": round(batch_seconds, 6),
                    "run": batch},
        "saved_processes": len(serial_runs) - 1,
        "saved_seconds": round(serial_seconds - batch_seconds, 6),
        "exact_list_stdout": exact, "list_stdout": stdout.decode("utf-8"),
    }
    write_json(OUT / "checker-efficiency.json", report)
    if batch["rc"] != 0 or batch["timeout"] or stderr or not exact:
        raise ValueError("batched efficiency query differs from the serial list")
    print(f"CASETAB efficiency: {len(names)} exact queries, "
          f"serial {len(serial_runs)} processes {serial_seconds:.6f}s, "
          f"batched 1 process {batch_seconds:.6f}s, "
          f"saved {len(serial_runs)-1} processes {serial_seconds-batch_seconds:.6f}s")


def verified_file(file, digest):
    candidate = path(file)
    if not re.fullmatch(r"[0-9a-f]{64}", digest) or sha256(candidate) != digest:
        raise ValueError(f"evidence hash mismatch: {file}")
    return candidate


def final():
    check_sync()
    manifest_path = path(os.environ.get("CASETAB_FINAL_MANIFEST", relative(OUT / "final.json")))
    manifest = json.loads(manifest_path.read_text())
    if manifest.get("schema") != "casetab-final-v1":
        raise ValueError("final manifest requires schema casetab-final-v1")
    checked = set()
    for entry in manifest.get("files", []):
        checked.add(relative(verified_file(entry["path"], entry["sha256"])))
    source_hashes = manifest.get("source_hashes", {})
    required_sources = set(SOURCES) | {
        PRELUDE, relative(Path(__file__)),
        "scripts/nelisp-standalone-build.el",
        "lisp/nelisp-stdlib-plist-str.el",
        "lisp/nelisp-stdlib-regexp.el",
        "lisp/nelisp-stdlib-unicode.el",
        "tools/nelisp-generate-unicode.el",
        "packages/nelisp-emacs-foundation/src/emacs-cc-case-region-1.el",
        "packages/nelisp-emacs-foundation/src/emacs-string.el",
        "test/nelisp-shadow-differential-cases.el",
    }
    required_sources |= {relative(fixture(mode)) for mode in MODES}
    required_sources |= {relative(expected(mode)) for mode in MODES}
    if not required_sources.issubset(source_hashes):
        raise ValueError("final source_hashes omit checker, fixtures or canonical sources")
    for file, digest in source_hashes.items():
        verified_file(file, digest)
    for kind in ("dynamic", "static"):
        recorded = manifest.get("binary_hashes", {}).get(kind, {})
        if recorded != identity(binary(kind)) or relative(binary(kind)) not in recorded:
            raise ValueError(f"final {kind} executable/companion identity changed")
        for mode in MODES:
            proof_path = OUT / kind / f"{mode}.json"
            if relative(proof_path) not in checked:
                raise ValueError(f"final files omit parity proof {relative(proof_path)}")
            proof = json.loads(proof_path.read_text())
            stdout = verified_file(proof["stdout"], proof["stdout_sha256"])
            stderr = verified_file(proof["stderr"], proof["stderr_sha256"])
            if (not proof.get("pass") or proof.get("rc") != 0 or
                    proof.get("timeout") or stderr.read_bytes() or
                    proof.get("binaries") != recorded or
                    proof.get("fixture_sha256") != sha256(fixture(mode)) or
                    proof.get("expected_sha256") != sha256(expected(mode)) or
                    stdout.read_bytes() != expected(mode).read_bytes()):
                raise ValueError(f"stale/failed parity proof: {kind} {mode}")
            rows(stdout.read_bytes(), mode)
    checks = manifest.get("checks", [])
    if len(checks) != 3 or {item.get("name") for item in checks} != {"common", "standalone", "mcp"}:
        raise ValueError("final requires common, standalone and mcp evidence")
    for item in checks:
        log = verified_file(item["log"], item["log_sha256"]).read_text()
        if item.get("rc") != 0 or item.get("failures") != 0 or item.get("count", 0) <= 0:
            raise ValueError(f"failed/empty final check: {item['name']}")
        if item["name"] == "common":
            if item["count"] != 11:
                raise ValueError("COMMON must run exactly 11 groups")
            summary = re.search(r"CASETAB-COMMON groups=11 failures=0\b", log)
            gate_counts = re.findall(r"GATE-COUNT: ([0-9]+) cases, ([0-9]+) failures", log)
            if (not summary and not re.search(r"11 groups[,; ]+0 failures", log)
                    and (not gate_counts or gate_counts[-1] != ("11", "0"))):
                raise ValueError("COMMON log lacks its 11 groups, 0 failures summary")
        elif item["name"] == "standalone":
            matches = re.findall(r"GATE-COUNT: ([0-9]+) cases, ([0-9]+) failures", log)
            if matches != [(str(item["count"]), "0")] or "STANDALONE-CHECKS PASS" not in log:
                raise ValueError("standalone log does not prove its claimed count/pass")
        else:
            if item["count"] != 40:
                raise ValueError("MCP must expose exactly 40 tools")
            # Store the actual JSON-RPC tools/list response, not a prose claim.
            responses = []
            for line in log.splitlines():
                try:
                    response = json.loads(line)
                except json.JSONDecodeError:
                    continue
                tools = response.get("result", {}).get("tools") if isinstance(response, dict) else None
                if isinstance(tools, list):
                    responses.append(tools)
            if len(responses) != 1 or len(responses[0]) != 40 or len({tool.get("name") for tool in responses[0]}) != 40:
                raise ValueError("MCP log lacks one tools/list response with 40 distinct tools")
    print(f"CASETAB final: 11 COMMON groups, {next(c['count'] for c in checks if c['name'] == 'standalone')} standalone cases, 40 MCP tools; hashes verified PASS")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=(*MODES, "final", "sync", "oracle", "base", "efficiency"))
    parser.add_argument("--timeout", type=float, default=float(os.environ.get("CASETAB_TIMEOUT", "20")))
    args = parser.parse_args()
    if args.mode == "sync":
        check_sync(update=True)
        print("CASETAB prelude sources synchronized")
    elif args.mode == "oracle":
        oracle(args.timeout)
    elif args.mode == "efficiency":
        efficiency(args.timeout)
    elif args.mode == "final":
        final()
    elif args.mode == "base":
        results = [run_mode(mode, "base", args.timeout) for mode in MODES]
        write_json(OUT / "red/summary.json", {"runs": results,
                   "expected_negative_control": all(not result["pass"] for result in results)})
        # Negative control is intentionally red, including its process status.
        return int(any(not result["pass"] for result in results))
    else:
        check_sync()
        results = [run_mode(args.mode, kind, args.timeout) for kind in ("dynamic", "static")]
        return int(any(not result["pass"] for result in results))
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, ValueError, KeyError, TypeError) as error:
        print(f"CASETAB FAIL: {error}", file=sys.stderr)
        sys.exit(1)
