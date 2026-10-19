#!/usr/bin/env python3
"""Record or verify the complete regex-speed S5 evidence bundle.

The checker runs no acceptance workload.  ``record`` accepts only already
complete outputs, hashes those outputs and their source/binary identities, and
writes the manifest atomically.  ``check`` validates the same bytes and
semantics later.  Use ``selftest`` for the three false-green controls.
"""

import argparse
import copy
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile


ROOT = Path(__file__).resolve().parents[2]
SCHEMA = 1
CORE_NAMES = (
    "standalone-checks",
    "standalone-shallow-binding-check",
    "standalone-shallow-binding-dyn-check",
    "standalone-fx-call-shape-check",
    "standalone-bytecode-word-check",
    "fx-spike",
    "fx-corpus",
    "thread-standalone",
    "thread-allocating",
    "thread-percpu",
    "thread-mirror-guard",
)
EVIDENCE_NAMES = (
    "parity", "parity_gnu", "native", "ert_focused", "ert_mvp",
    "ert_text_core", "core11", "mcp_json", "abi_ir",
)


class VerifyError(Exception):
    pass


def require(condition, message):
    if not condition:
        raise VerifyError(message)


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def stored_path(path, root=ROOT):
    path = path.resolve()
    try:
        return str(path.relative_to(root.resolve()))
    except ValueError:
        return str(path)


def resolve_path(value, root=ROOT):
    path = Path(value)
    return path if path.is_absolute() else root / path


def file_record(path, root=ROOT):
    path = Path(path).resolve()
    require(path.is_file(), f"missing file: {path}")
    size = path.stat().st_size
    require(size > 0, f"empty file: {path}")
    return {"path": stored_path(path, root), "size": size, "sha256": sha256(path)}


def verify_file_record(record, label, root=ROOT):
    require(isinstance(record, dict), f"{label}: missing record")
    require(set(record) == {"path", "size", "sha256"},
            f"{label}: malformed file record")
    path = resolve_path(record["path"], root)
    require(path.is_file(), f"{label}: missing {path}")
    require(path.stat().st_size > 0, f"{label}: empty {path}")
    require(path.stat().st_size == record["size"], f"{label}: stale size")
    require(sha256(path) == record["sha256"], f"{label}: stale hash")
    return path


def git_output(root, *args):
    return subprocess.check_output(
        ["git", "-C", str(root), *args], text=False).decode("utf-8")


def split_nul(output):
    return [item for item in output.split("\0") if item]


def source_snapshot(root=ROOT):
    tracked = set(split_nul(git_output(root, "diff", "--name-only", "-z", "HEAD", "--")))
    untracked = set(split_nul(git_output(
        root, "ls-files", "--others", "--exclude-standard", "-z", "--")))
    # Target products are evidence inputs, not authored source.  Every other
    # untracked path is explicit in the manifest, including new tests/tools.
    source_prefixes = ("lisp/", "packages/", "scripts/", "src/", "test/", "tools/")
    untracked = {name for name in untracked if name.startswith(source_prefixes)}
    rows = []
    for kind, names in (("tracked-modified", tracked), ("untracked", untracked)):
        for name in sorted(names):
            path = root / name
            require(path.is_file(), f"source state contains non-file: {name}")
            record = file_record(path, root)
            record["kind"] = kind
            record["mode"] = path.stat().st_mode & 0o777
            rows.append(record)
    return rows


def verify_source_state(expected, root=ROOT):
    require(isinstance(expected, list), "source_state: missing list")
    actual = source_snapshot(root)
    expected_shape = [(row.get("kind"), row.get("path")) for row in expected]
    actual_shape = [(row["kind"], row["path"]) for row in actual]
    require(expected_shape == actual_shape, "source state file list/status changed")
    for wanted, found in zip(expected, actual):
        require(wanted.get("size") == found["size"],
                f"source size changed: {found['path']}")
        require(wanted.get("sha256") == found["sha256"],
                f"source hash changed: {found['path']}")
        require(wanted.get("mode") == found["mode"],
                f"source mode changed: {found['path']}")


def parity_body(path, label):
    raw = path.read_bytes()
    marker = b"REGEX-PARITY-DONE\n"
    position = raw.find(marker)
    require(position >= 0, f"{label}: missing REGEX-PARITY-DONE")
    body = raw[:position]
    suffix = raw[position + len(marker):]
    # The standalone evaluator prints either the loaded file's `t' result or
    # the final `princ' form's string value.  Accept those known echoes only.
    require(suffix in (b"", b"t\n", b'"REGEX-PARITY-DONE\\n"\n',
                       b'"REGEX-PARITY-DONE\n"\n'),
            f"{label}: unexpected output after REGEX-PARITY-DONE")
    require(b"REGEX-PARITY-DONE" not in body,
            f"{label}: duplicate/embedded completion marker")
    require(body, f"{label}: empty parity body")
    return body


def validate_parity(parity, native, golden):
    golden_body = parity_body(golden, "GNU parity")
    require(parity_body(parity, "standalone parity") == golden_body,
            "standalone parity differs from GNU")
    require(parity_body(native, "native public parity") == golden_body,
            "native public parity differs from GNU")


def ert_summaries(text):
    return [(int(a), int(b), int(c)) for a, b, c in re.findall(
        r"Ran ([0-9]+) tests?, ([0-9]+) results? as expected, "
        r"([0-9]+) unexpected", text)]


def validate_ert(path, expected, label):
    text = path.read_text(errors="replace")
    summaries = ert_summaries(text)
    require(summaries, f"{label}: missing ERT summary")
    require((expected, expected, 0) in summaries,
            f"{label}: missing {expected}/{expected}/0 summary")
    require(all(total == passed and unexpected == 0
                for total, passed, unexpected in summaries),
            f"{label}: non-passing ERT summary")
    require(not re.search(r"^\s*FAILED\s", text, re.MULTILINE),
            f"{label}: FAILED test present")


def default_focused_count():
    total = 0
    for name in ("test/nelisp-regexp-diff-test.el",
                 "test/nelisp-regex-replacement-test.el"):
        text = (ROOT / name).read_text()
        total += len(re.findall(r"^[ \t]*\(ert-deftest\s+", text, re.MULTILINE))
    require(total > 0, "focused ERT source count is zero")
    return total


def validate_core(path):
    text = path.read_text(errors="replace")
    require("BOOT-IMAGE-FULL-SUITE mode=full checked=11 findings=0" in text,
            "core11: missing full 11/0 marker")
    seen = re.findall(r"^([a-z0-9-]+) rc=(-?[0-9]+)(?:\s|$)", text, re.MULTILINE)
    named = [(name, code) for name, code in seen if name in CORE_NAMES]
    require(named == [(name, "0") for name in CORE_NAMES],
            "core11: named groups must occur once, in order, with rc=0")


def validate_mcp(path, dynamic_hash):
    try:
        row = json.loads(path.read_text())
    except (ValueError, OSError) as error:
        raise VerifyError(f"MCP JSON: {error}") from error
    require(isinstance(row, dict), "MCP JSON: root is not an object")
    require(row.get("returncode") == 0, "MCP JSON: nonzero exit")
    require(row.get("initialized") is True, "MCP JSON: initialize did not succeed")
    require(row.get("tools_listed") is True, "MCP JSON: tools/list did not succeed")
    require(row.get("binary_unchanged") is True,
            "MCP JSON: binary changed during smoke")
    require(row.get("smoke_passed") is True, "MCP JSON: smoke did not pass")
    names = row.get("names")
    require(row.get("count") == 40, "MCP JSON: tool count is not 40")
    require(isinstance(names, list) and len(names) == 40,
            "MCP JSON: names do not contain 40 entries")
    require(len(set(names)) == 40 and all(isinstance(name, str) and name for name in names),
            "MCP JSON: tool names are empty or duplicated")
    require(row.get("binary_sha256") == dynamic_hash,
            "MCP JSON: binary identity differs from dynamic binary")


def validate_abi(path):
    text = path.read_text(errors="replace")
    pattern = re.compile(
        r"^Ir regex layout (0|2100) base ([1-9][0-9]*) cand ([1-9][0-9]*); "
        r"GC base ([0-9]+) cand ([0-9]+); output identical; delta [^\n]+$",
        re.MULTILINE)
    rows = pattern.findall(text)
    require(len(re.findall(r"^Ir regex layout ", text, re.MULTILINE)) == 2,
            "ABI Ir: unexpected extra layout rows")
    require([row[0] for row in rows] == ["0", "2100"],
            "ABI Ir: expected exactly layouts 0 and 2100")
    require(all(int(row[3]) >= 10 and int(row[4]) >= 10 for row in rows),
            "ABI Ir: each binary/layout needs at least 10 GC sweeps")
    require(sum(int(row[2]) for row in rows) <= sum(int(row[1]) for row in rows),
            "ABI Ir: candidate total exceeds baseline")
    aggregates = re.findall(
        r"^Ir regex mean base [0-9.]+ cand [0-9.]+; .*; "
        r"output identical; PASS$", text, re.MULTILINE)
    require(len(re.findall(r"^Ir regex mean ", text, re.MULTILINE)) == 1,
            "ABI Ir: expected exactly one aggregate row")
    require(len(aggregates) == 1, "ABI Ir: missing unique aggregate PASS")


def artifact_paths(dynamic, static):
    rows = {}
    for label, binary in (("dynamic", dynamic), ("static", static)):
        binary = Path(binary).resolve()
        rows[label] = binary
        rows[label + "_cold"] = Path(str(binary) + ".cold")
        rows[label + "_native_startup"] = Path(str(binary) + ".native-startup.el")
    return rows


def validate_inputs(paths, focused_count):
    require(len({path.resolve() for path in paths.values()}) == len(paths),
            "each evidence input must be a distinct file")
    validate_parity(paths["parity"], paths["native"], paths["parity_gnu"])
    validate_ert(paths["ert_focused"], focused_count, "focused ERT")
    validate_ert(paths["ert_mvp"], 56, "MVP ERT")
    validate_ert(paths["ert_text_core"], 27, "text-core ERT")
    validate_core(paths["core11"])
    validate_abi(paths["abi_ir"])


def record(args):
    source_focused_count = default_focused_count()
    focused_count = args.focused_count or source_focused_count
    require(focused_count == source_focused_count,
            f"focused count {focused_count} differs from source count "
            f"{source_focused_count}")
    artifacts = artifact_paths(args.dynamic, args.static)
    require(len({path.resolve() for path in artifacts.values()}) == len(artifacts),
            "dynamic/static artifacts and companions must be distinct")
    artifact_records = {name: file_record(path) for name, path in artifacts.items()}
    require(os.access(artifacts["dynamic"], os.X_OK), "dynamic binary is not executable")
    require(os.access(artifacts["static"], os.X_OK), "static binary is not executable")
    evidence_paths = {name: Path(getattr(args, name)).resolve() for name in EVIDENCE_NAMES}
    evidence_records = {name: file_record(path) for name, path in evidence_paths.items()}
    validate_inputs(evidence_paths, focused_count)
    validate_mcp(evidence_paths["mcp_json"], artifact_records["dynamic"]["sha256"])
    source_state = source_snapshot()
    for name, row in artifact_records.items():
        verify_file_record(row, f"artifact {name}")
    for name, row in evidence_records.items():
        verify_file_record(row, f"evidence {name}")
    require(source_snapshot() == source_state, "source state changed while recording")
    manifest = {
        "schema": SCHEMA,
        "recorded_at": datetime.now(timezone.utc).isoformat(),
        "git_head": git_output(ROOT, "rev-parse", "HEAD").strip(),
        "focused_count": focused_count,
        "artifacts": artifact_records,
        "evidence": evidence_records,
        "source_state": source_state,
    }
    args.manifest.parent.mkdir(parents=True, exist_ok=True)
    temporary = args.manifest.with_name(args.manifest.name + ".tmp")
    temporary.write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n")
    temporary.replace(args.manifest)
    print(f"REGEX-SPEED-VERIFY PASS record artifacts={len(artifact_records)} "
          f"evidence={len(evidence_records)} sources={len(manifest['source_state'])}")


def check(args):
    require(args.manifest.is_file(), f"missing manifest: {args.manifest}")
    require(args.manifest.stat().st_size > 0, f"empty manifest: {args.manifest}")
    try:
        manifest = json.loads(args.manifest.read_text())
    except ValueError as error:
        raise VerifyError(f"invalid manifest JSON: {error}") from error
    required = {"schema", "recorded_at", "git_head", "focused_count",
                "artifacts", "evidence", "source_state"}
    require(isinstance(manifest, dict) and set(manifest) == required,
            "manifest fields are missing or unexpected")
    require(manifest["schema"] == SCHEMA, "unsupported manifest schema")
    require(isinstance(manifest["recorded_at"], str) and manifest["recorded_at"],
            "missing recorded_at")
    require(manifest["git_head"] == git_output(ROOT, "rev-parse", "HEAD").strip(),
            "git HEAD changed")
    require(isinstance(manifest["focused_count"], int)
            and manifest["focused_count"] > 0, "invalid focused_count")
    expected_artifacts = set(artifact_paths("dynamic", "static"))
    require(set(manifest["artifacts"]) == expected_artifacts,
            "artifact records are missing or unexpected")
    require(set(manifest["evidence"]) == set(EVIDENCE_NAMES),
            "evidence records are missing or unexpected")
    artifact_files = {name: verify_file_record(row, f"artifact {name}")
                      for name, row in manifest["artifacts"].items()}
    evidence_files = {name: verify_file_record(row, f"evidence {name}")
                      for name, row in manifest["evidence"].items()}
    require(os.access(artifact_files["dynamic"], os.X_OK),
            "recorded dynamic binary is not executable")
    require(os.access(artifact_files["static"], os.X_OK),
            "recorded static binary is not executable")
    require(len({path.resolve() for path in artifact_files.values()})
            == len(artifact_files), "recorded artifacts are not distinct")
    validate_inputs(evidence_files, manifest["focused_count"])
    validate_mcp(evidence_files["mcp_json"],
                 manifest["artifacts"]["dynamic"]["sha256"])
    verify_source_state(manifest["source_state"])
    for name, row in manifest["artifacts"].items():
        verify_file_record(row, f"artifact {name}")
    for name, row in manifest["evidence"].items():
        verify_file_record(row, f"evidence {name}")
    verify_source_state(manifest["source_state"])
    print(f"REGEX-SPEED-VERIFY PASS check artifacts={len(artifact_files)} "
          f"evidence={len(evidence_files)} sources={len(manifest['source_state'])}")


def expect_rejection(label, function):
    try:
        function()
    except VerifyError:
        print(f"REGEX-SPEED-VERIFY NEGATIVE {label}: PASS")
        return
    raise VerifyError(f"negative control accepted: {label}")


def selftest(_args):
    with tempfile.TemporaryDirectory(prefix="regex-speed-verify-") as directory:
        tmp = Path(directory)
        good = tmp / "parity"
        native = tmp / "native"
        gnu = tmp / "gnu"
        for path in (good, native, gnu):
            path.write_text("case one\nREGEX-PARITY-DONE\n")
        bad_marker = tmp / "bad-marker"
        bad_marker.write_text("case one\n")
        expect_rejection("missing-completion-marker",
                         lambda: validate_parity(bad_marker, native, gnu))

        mcp = tmp / "mcp.json"
        mcp.write_text(json.dumps({
            "returncode": 0, "initialized": True, "count": 39,
            "names": [f"tool-{number}" for number in range(39)],
            "binary_sha256": "a" * 64, "binary_unchanged": True,
            "tools_listed": True, "smoke_passed": True,
        }))
        expect_rejection("wrong-mcp-count", lambda: validate_mcp(mcp, "a" * 64))

        repository = tmp / "repo"
        (repository / "tools").mkdir(parents=True)
        subprocess.run(["git", "init", "-q", str(repository)], check=True)
        (repository / "tools/fixture.py").write_text("value = 1\n")
        subprocess.run(["git", "-C", str(repository), "add", "tools/fixture.py"], check=True)
        subprocess.run([
            "git", "-C", str(repository), "-c", "user.name=Regex Verify",
            "-c", "user.email=regex-verify.invalid", "commit", "-qm", "fixture",
        ], check=True)
        (repository / "tools/fixture.py").write_text("value = 2\n")
        state = source_snapshot(repository)
        require(len(state) == 1, "selftest source fixture was not discovered")
        mutated = copy.deepcopy(state)
        mutated[0]["sha256"] = "0" * 64
        expect_rejection("source-hash-mutation",
                         lambda: verify_source_state(mutated, repository))
    print("REGEX-SPEED-VERIFY SELFTEST PASS controls=3")


def add_evidence_arguments(parser):
    parser.add_argument("--parity", required=True, type=Path)
    parser.add_argument("--parity-gnu", dest="parity_gnu", required=True, type=Path)
    parser.add_argument("--native", required=True, type=Path)
    parser.add_argument("--ert-focused", dest="ert_focused", required=True, type=Path)
    parser.add_argument("--ert-mvp", dest="ert_mvp", required=True, type=Path)
    parser.add_argument("--ert-text-core", dest="ert_text_core", required=True, type=Path)
    parser.add_argument("--core11", required=True, type=Path)
    parser.add_argument("--mcp-json", dest="mcp_json", required=True, type=Path)
    parser.add_argument("--abi-ir", dest="abi_ir", required=True, type=Path)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    recorder = commands.add_parser("record", help="validate evidence and write MANIFEST")
    recorder.add_argument("manifest", type=Path)
    recorder.add_argument("dynamic", type=Path)
    recorder.add_argument("static", type=Path)
    recorder.add_argument("--focused-count", type=int)
    add_evidence_arguments(recorder)
    checker = commands.add_parser("check", help="validate a recorded MANIFEST")
    checker.add_argument("manifest", type=Path)
    commands.add_parser("selftest", help="run false-green negative controls")
    args = parser.parse_args()
    {"record": record, "check": check, "selftest": selftest}[args.command](args)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (VerifyError, OSError, subprocess.CalledProcessError) as error:
        print(f"REGEX-SPEED-VERIFY FAIL: {error}", file=sys.stderr)
        raise SystemExit(1)
