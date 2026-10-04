#!/usr/bin/env python3
"""Run one C-core probe against host Emacs and a NeLisp binary."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import tempfile
import time


ROOT = Path(__file__).resolve().parents[2]
DRIVER = ROOT / "test/nelisp-emacs-lib/c-core-parity-driver.el"
PROBES = ROOT / "test/nelisp-emacs-lib/c-core-probes"
ACTIVE_ARTIFACT = None


def sha(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for block in iter(lambda: f.read(131072), b""):
            h.update(block)
    return h.hexdigest()


def resolve_file(value, label):
    path = Path(value)
    if not path.is_absolute():
        path = ROOT / path
    path = path.resolve()
    if not path.is_file():
        raise ValueError(f"{label} is not a file: {value}")
    return path


def lisp_string(value):
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n") + '"'


def harness_text(probe, sources):
    precheck = (";;; -*- lexical-binding: t; -*-\n(setq default-directory " + lisp_string(str(ROOT) + os.sep) + ")\n"
                "(let ((expected 0) (done nil)) (with-temp-buffer"
                " (insert-file-contents " + lisp_string(str(probe)) + ")"
                " (goto-char (point-min)) (while (not done)"
                " (skip-chars-forward \" \\t\\r\\n\")"
                " (while (eq (char-after) ?\\;) (forward-line 1)"
                " (skip-chars-forward \" \\t\\r\\n\"))"
                " (if (eobp) (setq done t)"
                " (let* ((entry (read (current-buffer))) (size (length entry)))"
                " (unless (and (consp entry) (symbolp (car entry)) (> size 1))"
                " (error \"Malformed probe entry\"))"
                " (setq expected (+ expected (1- size)))))))"
                " (princ (format \"P-EXPECTED|%d\\n\" expected)))\n")
    return precheck + "\n".join("(load " + lisp_string(str(p)) + " nil t)" for p in sources) + "\n"


def checked_output(stdout, stderr, returncode, label):
    if returncode != 0:
        raise ValueError(f"{label} exited {returncode}")
    if stderr:
        raise ValueError(f"{label} wrote unexpected stderr: {repr(stderr[-240:])}")
    try:
        text = stdout.decode("utf-8", "strict")
    except UnicodeDecodeError as exc:
        raise ValueError(f"{label} output is not UTF-8") from exc
    lines = text.splitlines(keepends=True)
    headers = [x for x in lines if x.startswith("P-EXPECTED|")]
    if len(headers) != 1 or not re.fullmatch(r"P-EXPECTED\|[0-9]+\n", headers[0]) or lines[0] != headers[0]:
        raise ValueError(f"{label} missing or malformed expected-count header")
    expected = int(headers[0].split("|", 1)[1])
    if "P-DONE\n" not in lines or sum(x == "P-DONE\n" for x in lines) != 1:
        tail = repr(text[-240:])
        raise ValueError(f"{label} missing or malformed P-DONE; output tail={tail}")
    marker = lines.index("P-DONE\n")
    if any(not (x.startswith("P| ") and x.endswith("\n")) for x in lines[1:marker]):
        raise ValueError(f"{label} emitted unexpected stdout before P-DONE")
    if lines[marker + 1:] not in ([], ["t\n"]):
        raise ValueError(f"{label} emitted unexpected stdout")
    probes = lines[1:marker]
    if expected < 1 or len(probes) != expected:
        raise ValueError(f"{label} probe count mismatch: expected={expected} actual={len(probes)}")
    return "".join([headers[0]] + probes + ["P-DONE\n"]).encode("utf-8"), len(probes)


def compare_transcripts(host, nelisp):
    if host[1] != nelisp[1] or host[0] != nelisp[0]:
        hs, ns = host[0].decode().splitlines(), nelisp[0].decode().splitlines()
        first = next(((a, b) for a, b in zip(hs, ns) if a != b), ("<end>", "<end>"))
        raise ValueError(f"parity mismatch: host={host[1]} NeLisp={nelisp[1]}; first={first!r}")


def identity_metadata(binary, cold, sources, probe, driver, runner, host_harness, nelisp_harness, identity):
    source_hash = hashlib.sha256()
    for path in sources:
        label = str(path.relative_to(ROOT)) if path.is_relative_to(ROOT) else path.name
        source_hash.update(label.encode() + b"\0" + sha(path).encode() + b"\0")
    fields = {"binary_sha256": sha(binary),
              "binary_cold_path": str(cold) if cold and cold.is_file() else None,
              "binary_cold_sha256": sha(cold) if cold and cold.is_file() else None,
              "source_sha256": source_hash.hexdigest(),
              "probe_sha256": sha(probe), "driver_sha256": sha(driver),
              "runner_sha256": sha(runner),
              "host_harness_sha256": hashlib.sha256(host_harness.encode()).hexdigest(),
              "nelisp_harness_sha256": hashlib.sha256(nelisp_harness.encode()).hexdigest(),
              "identity": identity}
    fields["test_identity_sha256"] = hashlib.sha256(json.dumps(fields, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
    return fields


def identity_matches(record, inputs):
    expected = identity_metadata(**inputs)
    return record == expected


def main():
    global ACTIVE_ARTIFACT
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--unit", required=True)
    ap.add_argument("--extra")
    ap.add_argument("--dependency", action="append", default=[])
    ap.add_argument("--artifact")
    ap.add_argument("--identity", choices=["tests"])
    ap.add_argument("binary", nargs="?", default=os.environ.get("NELISP_BIN"))
    args = ap.parse_args()
    if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_-]*", args.unit):
        ap.error("unsafe unit name")
    artifact = Path(args.artifact) if args.artifact else ROOT / "target/progress" / ("c-core-unit-" + args.unit + ".json")
    if not artifact.is_absolute():
        artifact = ROOT / artifact
    artifact.parent.mkdir(parents=True, exist_ok=True)
    ACTIVE_ARTIFACT = artifact
    artifact.write_text(json.dumps({"status": "RUNNING", "unit": args.unit}) + "\n", encoding="utf-8")
    if not args.binary:
        raise ValueError("NELISP_BIN or positional binary is required")
    probe = PROBES / (args.unit + ".el")
    impl = ROOT / "packages/nelisp-emacs-foundation/src" / ("emacs-cc-" + args.unit + ".el")
    if not probe.is_file() or not impl.is_file():
        ap.error(f"unit probe or implementation missing: {args.unit}")
    binary = resolve_file(args.binary, "binary")
    if not os.access(binary, os.X_OK):
        raise ValueError(f"binary is not executable: {args.binary}")
    extra = resolve_file(args.extra, "extra") if args.extra else None
    deps = [resolve_file(p, "dependency") for p in args.dependency]
    with tempfile.TemporaryDirectory(prefix="nelisp-c-core-") as td:
        work = Path(td)
        host_harness, nelisp_harness = work / "host.el", work / "nelisp.el"
        host_text = harness_text(probe, [DRIVER])
        nelisp_text = harness_text(probe, deps + [impl] + ([extra] if extra else []) + [DRIVER])
        host_harness.write_text(host_text, encoding="utf-8")
        nelisp_harness.write_text(nelisp_text, encoding="utf-8")
        env = os.environ.copy()
        env["C_CORE_UNIT"] = args.unit
        host = os.environ.get("EMACS", "emacs")
        started = time.monotonic()
        hr = subprocess.run([host, "-Q", "--batch", "-l", str(host_harness)], cwd=work, env=env,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=55)
        host_seconds = time.monotonic() - started
        started = time.monotonic()
        nr = subprocess.run([str(binary), "--load", str(nelisp_harness)], cwd=work, env=env,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=55)
        nelisp_seconds = time.monotonic() - started
        logs = {"host_stdout": str(artifact.with_suffix(".host.out")),
                "host_stderr": str(artifact.with_suffix(".host.err")),
                "nelisp_stdout": str(artifact.with_suffix(".nelisp.out")),
                "nelisp_stderr": str(artifact.with_suffix(".nelisp.err"))}
        for key, data in (("host_stdout", hr.stdout), ("host_stderr", hr.stderr),
                          ("nelisp_stdout", nr.stdout), ("nelisp_stderr", nr.stderr)):
            Path(logs[key]).write_bytes(data)
        ho, hn = checked_output(hr.stdout, hr.stderr, hr.returncode, "host")
        no, nn = checked_output(nr.stdout, nr.stderr, nr.returncode, "NeLisp")
        compare_transcripts((ho, hn), (no, nn))
        cold = Path(str(binary) + ".cold")
        source_files = deps + [impl] + ([extra] if extra else [])
        identity = identity_metadata(binary, cold, source_files, probe, DRIVER,
                                     Path(__file__).resolve(), host_text, nelisp_text, args.identity)
        result = {"status": "PASS", "unit": args.unit, "line_count": hn,
                          "host_seconds": round(host_seconds, 3), "nelisp_seconds": round(nelisp_seconds, 3),
                          "implementation_sha256": sha(impl),
                          **identity,
                          "logs": logs,
                          "dependencies": [{"path": str(p.relative_to(ROOT)) if p.is_relative_to(ROOT) else p.name,
                                            "sha256": sha(p)} for p in deps],
                          "extra_sha256": sha(extra) if extra else None}
        artifact.write_text(json.dumps(result, sort_keys=True, indent=2) + "\n", encoding="utf-8")
        result["artifact"] = str(artifact)
        print(json.dumps(result, sort_keys=True))


if __name__ == "__main__":
    try:
        main()
    except (ValueError, OSError, subprocess.TimeoutExpired) as exc:
        if ACTIVE_ARTIFACT is not None:
            try:
                ACTIVE_ARTIFACT.write_text(json.dumps({"status": "FAIL", "error": str(exc)}) + "\n", encoding="utf-8")
            except OSError:
                pass
        print(f"c-core-unit-smoke: FAIL: {exc}", file=__import__("sys").stderr)
        raise SystemExit(1)
