#!/usr/bin/env python3
"""Bind S4 candidate evidence to the live sources covered by its build guard.

capture   query both finished candidates and save independent source evidence
check     replay all hashes and recompute the host build-source identity
controls  reject real overlay edits and corrupted copies of captured inputs

Coverage is exactly the existing native-cache build guard: direct *.el entries
in src, lisp and scripts, plus vendor/staged-emacs-lisp/subr.el. Other vendor
files and recursively nested sources are outside this identity's coverage.
No existing object-probe or baseline evidence is imported or modified.
"""
from __future__ import annotations

import argparse
import copy
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
DEFAULT_MANIFEST = Path("target/word-heap-s4v/source-evidence.json")
DEFAULT_OUTPUT = Path("target/word-heap-s4v/source")
DIRECTORIES = ("src", "lisp", "scripts")
VENDOR = "vendor/staged-emacs-lisp/subr.el"
BUILD_SOURCE = "scripts/nelisp-standalone-build.el"
CANDIDATE_PATHS = {mode: f"target/word-heap-s4v/{mode}/nelisp-{mode}"
                   for mode in ("dynamic", "static")}
QUERY_SOURCE = b"(princ nelisp-native-cache--build-source-identity)\n(terpri)\n"
SCOPE = {"direct_el_directories": list(DIRECTORIES), "additional_files": [VENDOR],
         "algorithm": "sha256(GNU prin1-to-string(ordered (relative-path sha256) rows))"}
HOST_SOURCE = r''';;; Build-guard identity query. -*- lexical-binding: nil; -*-
;; Read only the real build-guard definition, without loading the builder.
(require 'json)
(let* ((root (file-name-as-directory (getenv "WHVS_SOURCE_ROOT")))
       (json-array-type 'list)
       (rows (json-read-file (getenv "WHVS_ROWS")))
       (definition nil))
  (with-temp-buffer
    (insert-file-contents (expand-file-name "scripts/nelisp-standalone-build.el" root))
    (goto-char (point-min))
    (while (not definition)
      (let ((form (read (current-buffer))))
        (when (and (consp form) (eq (car form) 'defun)
                   (eq (cadr form) 'nelisp-standalone--native-cache-source-identity))
          (setq definition form)))))
  (eval definition)
  (let* ((nelisp-standalone--repo-root root)
         (guard (nelisp-standalone--native-cache-source-identity))
         (listed (secure-hash 'sha256 (prin1-to-string rows))))
    (princ (json-encode `((guard . ,guard) (listed . ,listed))))
    (terpri)))
'''.encode()


def need(ok, message):
    if not ok:
        raise ValueError(message)


def sha(path):
    with Path(path).open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def relative(path):
    return Path(path).resolve().relative_to(ROOT).as_posix()


def local(path):
    path = Path(path)
    need(not path.is_absolute() and ".." not in path.parts, "Non-relative evidence path")
    return path


def write_json(path, value):
    Path(path).write_text(json.dumps(value, indent=2) + "\n")


def item(path):
    path = Path(path)
    need(path.is_file(), "Missing input: " + str(path))
    return {"path": relative(path), "sha256": sha(path), "bytes": path.stat().st_size}


def verify_item(row, overrides=None):
    path = local(row["path"])
    actual = (overrides or {}).get(path.as_posix(), ROOT / path)
    need(actual.is_file() and actual.stat().st_size == row["bytes"] and
         sha(actual) == row["sha256"], "Changed input: " + path.as_posix())
    return actual


def source_rows(root):
    rows = []
    for directory in DIRECTORIES:
        paths = sorted((root / directory).glob("*.el"))
        need(paths, "Empty source directory: " + directory)
        rows.extend([[path.relative_to(root).as_posix(), sha(path)] for path in paths])
    rows.append([VENDOR, sha(root / VENDOR)])
    return rows


def run_raw(directory, name, command, *, env=None, timeout=180):
    before = time.monotonic()
    try:
        run = subprocess.run(command, cwd=ROOT, env=env, capture_output=True, timeout=timeout)
        stdout, stderr, code = run.stdout, run.stderr, run.returncode
    except subprocess.TimeoutExpired as error:
        stdout, stderr, code = error.stdout or b"", error.stderr or b"", 124
    paths = {key: directory / (name + suffix) for key, suffix in
             (("stdout", ".out"), ("stderr", ".err"), ("status", ".rc"))}
    paths["stdout"].write_bytes(stdout)
    paths["stderr"].write_bytes(stderr)
    paths["status"].write_text(str(code) + "\n")
    return {"command": command, "exit": code, "seconds": time.monotonic() - before,
            **{key: item(path) for key, path in paths.items()}}


def replay_raw(row, overrides=None):
    paths = {key: verify_item(row[key], overrides) for key in ("stdout", "stderr", "status")}
    need(row["exit"] == 0 and paths["status"].read_bytes() == b"0\n" and
         paths["stderr"].read_bytes() == b"", "Unsuccessful identity query")
    return paths["stdout"].read_bytes()


def parse_host(stdout):
    value = json.loads(stdout)
    need(set(value) == {"guard", "listed"} and value["guard"] == value["listed"] and
         re.fullmatch(r"[0-9a-f]{64}", value["guard"]),
         "Build guard differs from the independently enumerated GNU prin1 rows")
    return value["guard"]


def host_identity(root, rows, directory, emacs):
    directory.mkdir(parents=True, exist_ok=True)
    host = directory / "host-identity.el"
    row_file = directory / "source-rows.json"
    host.write_bytes(HOST_SOURCE)
    write_json(row_file, rows)
    env = dict(os.environ, WHVS_SOURCE_ROOT=str(root), WHVS_ROWS=str(row_file.resolve()))
    command = [emacs, "-Q", "--batch", "-l", relative(host)]
    raw = run_raw(directory, "host-identity", command, env=env)
    identity = parse_host(replay_raw(raw))
    return identity, {"source": item(host), "rows": item(row_file), "raw": raw}


def validate_build(path):
    value = json.loads(path.read_text())
    need(type(value["rc"]) is int and value["rc"] == 0 and value["cold"] is True,
         "Candidate build must finish rc=0 and cold=true: " + relative(path))


def parse_candidate(stdout):
    need(re.fullmatch(rb"[0-9a-f]{64}\n", stdout) is not None,
         "Candidate identity stdout must be exactly 64 lowercase hex digits and newline")
    return stdout[:-1].decode("ascii")


def capture(args):
    manifest, directory = ROOT / local(args.manifest), ROOT / local(args.output)
    need(not manifest.exists(), "Capture manifest already exists; preserve it and choose a new path")
    need(not directory.exists(), "Capture output already exists; choose a fresh directory")
    directory.mkdir(parents=True)
    rows = source_rows(ROOT)
    identity, host = host_identity(ROOT, rows, directory, args.emacs)
    query = directory / "query-source-identity.el"
    query.write_bytes(QUERY_SOURCE)
    record = {"schema": 1, "scope": SCOPE, "source_rows": rows, "source_identity": identity,
              "checker": item(__file__), "query_source": item(query), "host": host,
              "candidates": {}}
    for mode in ("dynamic", "static"):
        binary = ROOT / CANDIDATE_PATHS[mode]
        build = binary.parent / "build-time.json"
        validate_build(build)
        artifacts = {suffix: item(Path(str(binary) + suffix))
                     for suffix in ("", ".cold", ".native-startup.el")}
        need(all(row["bytes"] > 0 for row in artifacts.values()), "Empty candidate artifact")
        env = {"PATH": os.environ.get("PATH", os.defpath), "LANG": "C.UTF-8",
               "LC_ALL": "C.UTF-8", "TMPDIR": str(ROOT / "target/tmp"),
               "XDG_CACHE_HOME": str(directory / (mode + "-cache"))}
        # An explicit nil final action suppresses the CLI's default printing of
        # load's return value, keeping stdout solely the query's 64-byte digest.
        command = [relative(binary), "--load", relative(query), "--eval", "nil"]
        raw = run_raw(directory, mode + "-identity", command, env=env, timeout=args.timeout)
        need(parse_candidate(replay_raw(raw)) == identity,
             mode + " embedded identity does not match current build-guard sources")
        record["candidates"][mode] = {"artifacts": artifacts, "build": item(build), "query": raw}
    # Re-read everything before publishing so a concurrent build/source edit cannot
    # mix capture generations. The normal check performs these same comparisons.
    check_record(record, args.emacs)
    manifest.parent.mkdir(parents=True, exist_ok=True)
    with manifest.open("x") as stream:
        stream.write(json.dumps(record, indent=2) + "\n")
    print(f"WORD-HEAP S4 SOURCE CAPTURE PASS files={len(rows)} candidates=2 identity={identity}")


def check_record(record, emacs, source_root=ROOT, overrides=None):
    need(record["schema"] == 1 and record["scope"] == SCOPE, "Wrong source evidence schema/scope")
    verify_item(record["checker"], overrides)
    need(record["checker"]["path"] == relative(__file__), "Wrong source checker")
    current = source_rows(source_root)
    saved = record["source_rows"]
    need([row[0] for row in current] == [row[0] for row in saved], "Build-guard source file set changed")
    need(current == saved, "Build-guard source bytes changed")
    query = verify_item(record["query_source"], overrides)
    need(query.read_bytes() == QUERY_SOURCE, "Wrong candidate identity query source")
    host = record["host"]
    need(verify_item(host["source"], overrides).read_bytes() == HOST_SOURCE, "Wrong host identity source")
    need(json.loads(verify_item(host["rows"], overrides).read_text()) == saved, "Wrong captured source rows")
    need(parse_host(replay_raw(host["raw"], overrides)) == record["source_identity"],
         "Captured host guard identity changed")
    with tempfile.TemporaryDirectory(prefix="wh-source-check-", dir=ROOT / "target/tmp") as temp:
        identity, _ = host_identity(source_root, current, Path(temp), emacs)
    need(identity == record["source_identity"], "Current build-guard identity changed")
    need(set(record["candidates"]) == {"dynamic", "static"}, "Both candidate linkages are required")
    binaries = [value["artifacts"][""] for value in record["candidates"].values()]
    need(len({value["path"] for value in binaries}) == 2 and
         len({value["sha256"] for value in binaries}) == 2, "Candidate linkages must use distinct binaries")
    for mode, candidate in record["candidates"].items():
        artifacts = candidate["artifacts"]
        need(set(artifacts) == {"", ".cold", ".native-startup.el"}, "Incomplete candidate trio")
        binary = artifacts[""]["path"]
        need(binary == CANDIDATE_PATHS[mode], "Source evidence must use the measured S4 candidate paths")
        for suffix, row in artifacts.items():
            need(row["path"] == binary + suffix and row["bytes"] > 0, "Wrong candidate trio path")
            verify_item(row, overrides)
        build = verify_item(candidate["build"], overrides)
        need(candidate["build"]["path"] == str(local(binary).parent / "build-time.json"),
             "Wrong candidate build receipt")
        validate_build(build)
        raw = candidate["query"]
        need(raw["command"] == [binary, "--load", record["query_source"]["path"], "--eval", "nil"],
             "Wrong candidate identity query command")
        need(parse_candidate(replay_raw(raw, overrides)) == identity,
             mode + " captured embedded identity differs from live source identity")
    return len(current)


def controls(args):
    record = json.loads((ROOT / local(args.manifest)).read_text())
    checked = check_record(record, args.emacs)
    directory = ROOT / local(args.output)
    directory.mkdir(parents=True, exist_ok=True)
    results = {}
    mutations = {}

    def reject(name, *, root=ROOT, overrides=None):
        try:
            check_record(record, args.emacs, root, overrides)
        except ValueError as error:
            results[name] = str(error)
        else:
            raise ValueError("Negative control escaped: " + name)

    with tempfile.TemporaryDirectory(prefix="wh-source-controls-", dir=ROOT / "target/tmp") as temp:
        overlay = Path(temp) / "sources"
        for path, _ in record["source_rows"]:
            target = overlay / path
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(ROOT / path, target)
        need(source_rows(overlay) == record["source_rows"], "Source overlay did not reproduce input set")
        edited = overlay / record["source_rows"][0][0]
        original = edited.read_bytes()
        edited.write_bytes(original + b"\n;; S4 source-closure negative control.\n")
        mutations["source-bytes"] = {"path": record["source_rows"][0][0],
                                      "before": record["source_rows"][0][1], "after": sha(edited)}
        reject("source-bytes", root=overlay)
        edited.write_bytes(original)
        added = overlay / "src/wh-source-control-added.el"
        need(not added.exists(), "Source-addition control path already exists")
        added.write_text("nil\n")
        mutations["source-added"] = {"path": added.relative_to(overlay).as_posix(), "sha256": sha(added)}
        reject("source-added", root=overlay)
        added.unlink()
        edited.unlink()
        mutations["source-removed"] = {"path": record["source_rows"][0][0], "present_after": edited.exists()}
        reject("source-removed", root=overlay)
        edited.write_bytes(original)
        binary = record["candidates"]["dynamic"]["artifacts"][""]["path"]
        broken = Path(temp) / "broken-binary"
        shutil.copyfile(ROOT / binary, broken)
        with broken.open("r+b") as stream:
            first = stream.read(1)
            stream.seek(0)
            stream.write(bytes([first[0] ^ 1]))
        mutations["binary-bytes"] = {"path": binary, "before": sha(ROOT / binary), "after": sha(broken)}
        reject("binary-bytes", overrides={binary: broken})
        raw = record["candidates"]["dynamic"]["query"]["stdout"]["path"]
        broken_raw = Path(temp) / "broken-identity.out"
        stdout = (ROOT / raw).read_bytes()
        broken_raw.write_bytes((b"1" if stdout[:1] == b"0" else b"0") + stdout[1:])
        mutations["identity-raw"] = {"path": raw, "before": sha(ROOT / raw), "after": sha(broken_raw)}
        reject("identity-raw", overrides={raw: broken_raw})
        # Even re-hashing a forged, syntactically valid identity cannot make it
        # agree with the independently recomputed live source guard.
        forged = copy.deepcopy(record)
        forged["candidates"]["dynamic"]["query"]["stdout"]["sha256"] = sha(broken_raw)
        try:
            check_record(forged, args.emacs, overrides={raw: broken_raw})
        except ValueError as error:
            results["identity-raw-rehashed"] = str(error)
        else:
            raise ValueError("Re-hashed identity corruption escaped")
    need(len(results) == 6, "Incomplete source-closure controls")
    need(check_record(record, args.emacs) == checked, "Clean source replay changed after controls")
    write_json(directory / "controls.json", {"checked_sources": checked, "rejected": results,
                                             "mutations": mutations,
                                             "clean_replay": "PASS"})
    print(f"WORD-HEAP S4 SOURCE CONTROLS PASS files={checked} rejected={len(results)}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("mode", choices=("capture", "check", "controls"))
    parser.add_argument("--manifest", type=Path, default=DEFAULT_MANIFEST)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--emacs", default="emacs")
    parser.add_argument("--timeout", type=float, default=180)
    args = parser.parse_args()
    args.output = args.output or (Path("target/word-heap-s4v/source-controls")
                                  if args.mode == "controls" else DEFAULT_OUTPUT)
    if args.mode == "capture":
        capture(args)
    elif args.mode == "controls":
        controls(args)
    else:
        record = json.loads((ROOT / local(args.manifest)).read_text())
        count = check_record(record, args.emacs)
        print(f"WORD-HEAP S4 SOURCE CHECK PASS files={count} candidates=2 identity={record['source_identity']}")


if __name__ == "__main__":
    try:
        main()
    except (ValueError, OSError, KeyError, TypeError, subprocess.SubprocessError) as error:
        print(f"WORD-HEAP S4 SOURCE FAIL: {error}", file=sys.stderr)
        sys.exit(1)
