#!/usr/bin/env python3
"""Focused negative and contract controls for c-core-unit-smoke.py."""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import os


RUNNER = Path(__file__).with_name("c-core-unit-smoke.py")
SPEC = importlib.util.spec_from_file_location("ccore_runner", RUNNER)
runner = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(runner)


def accepted(stdout=b"P-EXPECTED|1\nP| probe | t\nP-DONE\n", stderr=b"", rc=0):
    return runner.checked_output(stdout, stderr, rc, "fixture")


def rejected(stdout=b"P| probe | t\nP-DONE\n", stderr=b"", rc=0):
    try:
        accepted(stdout, stderr, rc)
    except ValueError:
        return True
    return False


def identity_control():
    with tempfile.TemporaryDirectory(prefix="ccore-identity-") as td:
        root = Path(td)
        paths = {name: root / name for name in ("runtime", "source.el", "probe.el", "driver.el", "runner.py")}
        for name, path in paths.items():
            path.write_bytes(("fixture:" + name).encode())
        cold = Path(str(paths["runtime"]) + ".cold")
        cold.write_bytes(b"fixture:cold")
        inputs = {"binary": paths["runtime"], "cold": cold, "sources": [paths["source.el"]],
                  "probe": paths["probe.el"], "driver": paths["driver.el"],
                  "runner": paths["runner.py"], "host_harness": "host harness fixture",
                  "nelisp_harness": "nelisp harness fixture", "identity": "tests"}
        actual = runner.identity_metadata(**inputs)
        filehash = lambda name: hashlib.sha256(paths[name].read_bytes()).hexdigest()
        expected = {"binary_sha256": filehash("runtime"), "binary_cold_path": str(cold),
                    "binary_cold_sha256": hashlib.sha256(cold.read_bytes()).hexdigest(),
                    "source_sha256": hashlib.sha256(("source.el\0" + filehash("source.el") + "\0").encode()).hexdigest(),
                    "probe_sha256": filehash("probe.el"), "driver_sha256": filehash("driver.el"),
                    "runner_sha256": filehash("runner.py"),
                    "host_harness_sha256": hashlib.sha256(inputs["host_harness"].encode()).hexdigest(),
                    "nelisp_harness_sha256": hashlib.sha256(inputs["nelisp_harness"].encode()).hexdigest(),
                    "identity": "tests"}
        expected["test_identity_sha256"] = hashlib.sha256(json.dumps(expected, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
        binding_ok = actual == expected and runner.identity_matches(actual, inputs)
        changed_rejected = True
        for name in paths:
            before = paths[name].read_bytes()
            paths[name].write_bytes(before + b":changed")
            changed_rejected = changed_rejected and not runner.identity_matches(actual, inputs)
            paths[name].write_bytes(before)
        for key in ("host_harness", "nelisp_harness"):
            before = inputs[key]
            inputs[key] += ":changed"
            changed_rejected = changed_rejected and not runner.identity_matches(actual, inputs)
            inputs[key] = before
        damaged = dict(actual)
        damaged["test_identity_sha256"] = "0" * 64
        return binding_ok and changed_rejected and not runner.identity_matches(damaged, inputs)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--identity", action="store_true", help="run the identity hash-binding control only")
    args = parser.parse_args()
    if args.identity:
        ok = identity_control()
        print(json.dumps({"checks": {"identity_hash_binding": ok}, "passed": int(ok), "total": 1}, sort_keys=True))
        return 0 if ok else 1
    checks = {
        "valid_transcript": accepted()[1] == 1,
        "missing_marker": rejected(b"P-EXPECTED|1\nP| probe | t\n"),
        "nonzero_exit": rejected(rc=3),
        "unexpected_stderr": rejected(stderr=b"warning\n"),
        "zero_lines": rejected(b"P-EXPECTED|0\nP-DONE\n"),
        "missing_expected_count": rejected(b"P| probe | t\nP-DONE\n"),
        "mismatched_expected_count": rejected(b"P-EXPECTED|2\nP| probe | t\nP-DONE\n"),
        "malformed_expected_count": rejected(b"P-EXPECTED|one\nP| probe | t\nP-DONE\n"),
        "unexpected_stdout": rejected(b"noise\nP-EXPECTED|1\nP| probe | t\nP-DONE\n"),
        "duplicate_marker": rejected(b"P-EXPECTED|1\nP| probe | t\nP-DONE\nP-DONE\n"),
    }
    good = accepted()
    changed = accepted(b"P-EXPECTED|1\nP| probe | nil\nP-DONE\n")
    try:
        runner.compare_transcripts(good, changed)
        checks["changed_probe_red"] = False
    except ValueError:
        checks["changed_probe_red"] = True
    for label, unit in (("unsafe_unit_rejected", "../category-1"), ("missing_unit_rejected", "no-such-unit")):
        result = subprocess.run(["python3", str(RUNNER), "--unit", unit, "/bin/true"],
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        checks[label] = result.returncode != 0
    with tempfile.TemporaryDirectory(prefix="ccore-old-unavailable-") as td:
        root = Path(td)
        lib = root / "test/nelisp-emacs-lib"
        lib.mkdir(parents=True)
        probes = lib / "c-core-probes"
        probes.mkdir()
        shutil.copy2(RUNNER.parent / "c-core-probes/category-1.el", probes / "category-1.el")
        shutil.copy2(RUNNER.parent / "c-core-parity-smoke.sh", lib / "c-core-parity-smoke.sh")
        result = subprocess.run(["bash", str(lib / "c-core-parity-smoke.sh"), "run", "--unit", "category-1"],
                                cwd=root, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        checks["old_bundle_unavailable_red"] = result.returncode != 0 and "build/nemacs-bootstrap.el missing" in result.stdout
    with tempfile.TemporaryDirectory(prefix="ccore-malformed-probe-") as td:
        bad = Path(td) / "bad.el"
        bad.write_text("(broken (form\n", encoding="utf-8")
        harness = Path(td) / "check.el"
        harness.write_text(runner.harness_text(bad, [RUNNER.parent / "c-core-parity-driver.el"]), encoding="utf-8")
        result = subprocess.run(["emacs", "-Q", "--batch", "-l", str(harness)], text=True,
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        checks["malformed_probe_eof_red"] = result.returncode != 0
        empty = Path(td) / "empty.el"
        empty.write_text(" ; a valid empty probe\n", encoding="utf-8")
        empty_harness = Path(td) / "empty.elisp"
        empty_harness.write_text(runner.harness_text(empty, []), encoding="utf-8")
        result = subprocess.run(["emacs", "-Q", "--batch", "-l", str(empty_harness)],
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        checks["valid_empty_probe_parses_but_cannot_prove"] = (result.returncode == 0
            and result.stdout == b"P-EXPECTED|0\n"
            and rejected(result.stdout + b"P-DONE\n"))
    with tempfile.TemporaryDirectory(prefix="ccore-stale-artifact-") as td:
        proof = Path(td) / "proof.json"
        proof.write_text('{"status":"PASS"}\n', encoding="utf-8")
        result = subprocess.run(["python3", str(RUNNER), "--unit", "casetab-1", "--artifact", str(proof), "/bin/false"],
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        checks["failed_rerun_invalidates_old_pass"] = result.returncode != 0 and json.loads(proof.read_text()).get("status") == "FAIL"
        no_binary_env = {k: v for k, v in __import__("os").environ.items() if k != "NELISP_BIN"}
        missing_proof = Path(td) / "missing-binary.json"
        result = subprocess.run(["python3", str(RUNNER), "--unit", "casetab-1", "--artifact", str(missing_proof)],
                                env=no_binary_env, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        checks["missing_NELISP_BIN_rejected"] = result.returncode != 0 and json.loads(missing_proof.read_text()).get("status") == "FAIL"
        not_executable = Path(td) / "not-executable"
        not_executable.write_text("binary fixture", encoding="utf-8")
        nonexec_proof = Path(td) / "non-executable.json"
        result = subprocess.run(["python3", str(RUNNER), "--unit", "casetab-1", "--artifact", str(nonexec_proof), str(not_executable)],
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        checks["non_executable_binary_rejected"] = result.returncode != 0 and json.loads(nonexec_proof.read_text()).get("status") == "FAIL"
    checks["identity_hash_binding"] = identity_control()
    print(json.dumps({"checks": checks, "passed": sum(checks.values()), "total": len(checks)}, sort_keys=True))
    return 0 if all(checks.values()) else 1


if __name__ == "__main__":
    raise SystemExit(main())
