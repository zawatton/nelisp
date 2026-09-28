#!/usr/bin/env python3
"""Build and probe a pinned standalone candidate in an isolated P5 capsule."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import time


SOURCE_DIRS = ("scripts", "lisp", "src", "packages", "vendor", "test", "tools",
               "standalone-compat")
SOURCE_FILES = ("Makefile",)
EXCLUDED_DIRS = {"target", ".git", "node_modules", "__pycache__", ".serena"}


class CapsuleError(Exception):
    def __init__(self, stage, detail):
        super().__init__(detail)
        self.stage = stage
        self.detail = detail


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def source_files(root):
    """List every input copied into the build snapshot, in stable order."""
    paths = []
    for name in SOURCE_FILES:
        path = root / name
        if not path.is_file() or path.is_symlink():
            raise CapsuleError("snapshot", f"missing or linked build input: {path}")
        paths.append(path)
    for name in SOURCE_DIRS:
        directory = root / name
        if not directory.is_dir() or directory.is_symlink():
            raise CapsuleError("snapshot", f"missing or linked source directory: {directory}")
        for base, dirs, files in os.walk(directory):
            kept_dirs = []
            for item in dirs:
                if item in EXCLUDED_DIRS:
                    continue
                path = Path(base) / item
                if path.is_symlink():
                    raise CapsuleError("snapshot", f"linked source directory: {path}")
                kept_dirs.append(item)
            dirs[:] = sorted(kept_dirs)
            for filename in sorted(files):
                path = Path(base) / filename
                if path.is_symlink():
                    raise CapsuleError("snapshot", f"linked source input: {path}")
                if path.is_file():
                    paths.append(path)
    return sorted(paths)


def source_manifest(root):
    return {str(path.relative_to(root)): sha256(path) for path in source_files(root)}


def copy_source(source, snapshot):
    snapshot.mkdir(parents=True)
    for name in SOURCE_FILES:
        shutil.copy2(source / name, snapshot / name)
    for name in SOURCE_DIRS:
        shutil.copytree(source / name, snapshot / name,
                        ignore=shutil.ignore_patterns(*EXCLUDED_DIRS))


def check_hashes(stage, expected, current):
    for path in sorted(set(expected) | set(current)):
        if path not in current:
            raise CapsuleError(stage, f"missing input: {path}")
        if path not in expected:
            raise CapsuleError(stage, f"new input: {path}")
        if expected[path] != current[path]:
            raise CapsuleError(stage, f"hash changed: {path}")


def executable_inputs():
    """Hash the directly invoked build/probe tools and the runner itself."""
    paths = {"runner": Path(__file__).resolve(),
             "python": Path(sys.executable).resolve()}
    for name in ("make", "emacs", "cc", "ld", "bash", "git"):
        found = shutil.which(name)
        if not found:
            raise CapsuleError("preflight", f"missing build tool: {name}")
        paths[name] = Path(found).resolve()
    return {name: {"path": str(path), "sha256": sha256(path)}
            for name, path in paths.items()}


def verify_inputs(stage, source, snapshot, source_hashes, snapshot_hashes,
                  binary, binary_hash, patch, patch_hash, tools, candidate=None,
                  candidate_hash=None):
    check_hashes(stage, source_hashes, source_manifest(source))
    check_hashes(stage, snapshot_hashes, source_manifest(snapshot))
    for label, path, expected in (("binary", binary, binary_hash),
                                  ("patch", patch, patch_hash),
                                  ("candidate", candidate, candidate_hash)):
        if path is not None and expected is not None and (
                not path.is_file() or sha256(path) != expected):
            raise CapsuleError(stage, f"missing or changed {label}: {path}")
    for name, row in tools.items():
        path = Path(row["path"])
        if not path.is_file() or sha256(path) != row["sha256"]:
            raise CapsuleError(stage, f"missing or changed tool: {name}: {path}")


def guarded_command(stage, command, cwd, env, verify, timeout, on_start=None):
    verify(stage + ":before")
    started = time.monotonic()
    process = subprocess.Popen(command, cwd=cwd, env=env, text=True,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    try:
        if on_start:
            on_start()
        while True:
            try:
                stdout, stderr = process.communicate(timeout=0.25)
                break
            except subprocess.TimeoutExpired:
                verify(stage + ":during")
                if time.monotonic() - started > timeout:
                    raise CapsuleError(stage, f"timeout after {timeout}s")
        verify(stage + ":after")
    except BaseException:
        if process.poll() is None:
            process.kill()
            process.communicate()
        raise
    return {"returncode": process.returncode,
            "elapsed_ms": round(1000 * (time.monotonic() - started), 1),
            "stdout": stdout.strip()[-500:], "stderr": stderr.strip()[-500:],
            "uncaught_error": "uncaught error:" in stderr}


def run_capsule(source, binary, output_root, probe, patch=None,
                after_snapshot=None, after_probe_start=None):
    """Run a capsule.  Callbacks are used only by the negative-control tests."""
    source, binary, output_root = Path(source).resolve(), Path(binary).resolve(), Path(output_root).resolve()
    patch = Path(patch).resolve() if patch else None
    output_root.mkdir(parents=True, exist_ok=True)
    run_dir = Path(tempfile.mkdtemp(prefix="run-", dir=output_root))
    snapshot = run_dir / "source"
    build_dir = snapshot / "target"
    tmp = run_dir / "tmp"
    tmp.mkdir()
    candidate = build_dir / "nelisp"
    report_path = run_dir / "report.json"
    report = {"schema": 1, "status": "failed", "run_dir": str(run_dir),
              "source_worktree": str(source), "input_binary": str(binary),
              "candidate_binary": str(candidate), "snapshot": str(snapshot),
              "patch": str(patch) if patch else None, "probe": probe,
              "first_failing_stage": None, "stages": {}}
    started = time.monotonic()
    stage = "preflight"
    try:
        if not source.is_dir() or not binary.is_file() or not os.access(binary, os.X_OK):
            raise CapsuleError(stage, "missing source worktree or executable binary")
        if patch and not patch.is_file():
            raise CapsuleError(stage, f"missing patch: {patch}")
        if Path(probe).is_absolute() or ".." in Path(probe).parts:
            raise CapsuleError(stage, "probe must be a path inside the snapshot")
        binary_hash = sha256(binary)
        patch_hash = sha256(patch) if patch else None
        tools = executable_inputs()
        source_hashes = source_manifest(source)
        stage = "snapshot"
        copy_source(source, snapshot)
        build_dir.mkdir()
        check_hashes(stage, source_hashes, source_manifest(snapshot))
        if patch:
            subprocess.run([tools["git"]["path"], "apply", "--check", str(patch)], cwd=snapshot,
                           check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            subprocess.run([tools["git"]["path"], "apply", str(patch)], cwd=snapshot,
                           check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        snapshot_hashes = source_manifest(snapshot)
        if not (snapshot / probe).is_file():
            raise CapsuleError(stage, f"missing focused probe: {probe}")
        if after_snapshot:
            after_snapshot(source, snapshot)
        manifest = {"source": source_hashes, "snapshot": snapshot_hashes,
                    "input_binary": {str(binary): binary_hash},
                    "patch": {str(patch): patch_hash} if patch else {},
                    "tools": tools}
        manifest_path = run_dir / "input-manifest.json"
        manifest_path.write_text(json.dumps(manifest, sort_keys=True, separators=(",", ":")) + "\n")
        report.update({"input_manifest": str(manifest_path),
                       "input_manifest_sha256": sha256(manifest_path),
                       "source_file_count": len(source_hashes),
                       "snapshot_file_count": len(snapshot_hashes),
                       "binary_sha256": binary_hash, "patch_sha256": patch_hash})
        candidate_hash = None

        def verify(check_stage):
            verify_inputs(check_stage, source, snapshot, source_hashes,
                          snapshot_hashes, binary, binary_hash, patch,
                          patch_hash, tools, candidate,
                          candidate_hash)

        env = {"PATH": os.environ.get("PATH", "/usr/bin:/bin"),
               "HOME": str(tmp), "LC_ALL": "C",
               "EMACS": tools["emacs"]["path"],
               "CC": tools["cc"]["path"], "LD": tools["ld"]["path"]}
        env.update({"NELISP_ROOT": str(snapshot), "TMPDIR": str(tmp),
                    "NELISP_STANDALONE_READER_OUTPUT": str(candidate),
                    "NELISP_BIN": str(candidate),
                    "PYTHONDONTWRITEBYTECODE": "1"})
        stage = "baseline"
        baseline = guarded_command(stage, [str(binary), "--eval", "(cadr '(1 2 3))"],
                                   snapshot, env, verify, 20)
        report["stages"][stage] = baseline
        if (baseline["returncode"] != 0 or baseline["stdout"] != "2"
                or baseline["uncaught_error"]):
            raise CapsuleError(stage, "startup probe did not return 2")
        stage = "build"
        build = guarded_command(stage, [tools["make"]["path"], "standalone-reader"],
                                snapshot, env, verify, 180)
        report["stages"][stage] = build
        if build["returncode"] != 0 or not candidate.is_file():
            raise CapsuleError(stage, "candidate build failed")
        candidate_hash = sha256(candidate)
        report["candidate_sha256"] = candidate_hash
        stage = "focused_probe"
        focus = guarded_command(stage, [tools["bash"]["path"], str(snapshot / probe),
                                        str(candidate)], snapshot, env, verify, 90,
                                on_start=(lambda: after_probe_start(candidate))
                                if after_probe_start else None)
        report["stages"][stage] = focus
        if (focus["returncode"] != 0 or "PASS" not in focus["stdout"]
                or focus["uncaught_error"]):
            raise CapsuleError(stage, "focused probe failed, omitted PASS, or emitted an uncaught error")
        verify("final")
        report["status"] = "passed"
    except (CapsuleError, OSError, subprocess.SubprocessError) as error:
        report["first_failing_stage"] = error.stage if isinstance(error, CapsuleError) else stage
        report["failure"] = str(error)
    report["elapsed_ms"] = round(1000 * (time.monotonic() - started), 1)
    report_path.write_text(json.dumps(report, sort_keys=True, separators=(",", ":")) + "\n")
    return report_path, report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-worktree", required=True)
    parser.add_argument("--binary", required=True)
    parser.add_argument("--patch")
    parser.add_argument("--output-root", required=True)
    parser.add_argument("--probe", default="test/standalone-bytecode-jit-cadr-return-smoke.sh")
    args = parser.parse_args()
    path, report = run_capsule(args.source_worktree, args.binary,
                               args.output_root, args.probe, args.patch)
    print(path)
    return 0 if report["status"] == "passed" else 1


if __name__ == "__main__":
    sys.exit(main())
