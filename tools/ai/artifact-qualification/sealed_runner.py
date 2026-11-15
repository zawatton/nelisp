#!/usr/bin/env python3
"""Run a validated input bundle in a fresh, read-only Bubblewrap namespace."""

from __future__ import annotations

import argparse
import errno
import fcntl
import hashlib
import os
import shutil
import stat
import subprocess
import sys
from pathlib import Path, PurePosixPath

from artifact_qualifier import BundleError, validate_bundle


class RunnerError(RuntimeError):
    pass


class BundleTimeout(RunnerError):
    pass


def _read_file_nofollow(root: Path, relative: str, expected_mode: str, expected_hash: str) -> bytes:
    """Read one regular input through no-follow directory descriptors."""
    parts = PurePosixPath(relative).parts
    if not parts or PurePosixPath(relative).is_absolute() or ".." in parts:
        raise RunnerError(f"invalid source path: {relative}")
    dirfd = os.open(root, os.O_RDONLY | os.O_DIRECTORY)
    try:
        for part in parts[:-1]:
            nextfd = os.open(part, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW, dir_fd=dirfd)
            os.close(dirfd)
            dirfd = nextfd
        fd = os.open(parts[-1], os.O_RDONLY | os.O_NOFOLLOW, dir_fd=dirfd)
    except OSError as exc:
        raise RunnerError(f"cannot securely open input {relative}: {exc}") from exc
    finally:
        os.close(dirfd)
    try:
        st = os.fstat(fd)
        if not stat.S_ISREG(st.st_mode) or stat.S_IMODE(st.st_mode) != int(expected_mode, 8):
            raise RunnerError(f"input type/mode changed after validation: {relative}")
        chunks = []
        while True:
            chunk = os.read(fd, 1024 * 1024)
            if not chunk:
                break
            chunks.append(chunk)
        data = b"".join(chunks)
    finally:
        os.close(fd)
    if hashlib.sha256(data).hexdigest() != expected_hash:
        raise RunnerError(f"input bytes changed after validation: {relative}")
    return data


def _sealed_memfd(label: str, data: bytes) -> int:
    if not hasattr(os, "memfd_create"):
        raise RunnerError("sealed memfd support is unavailable")
    fd = os.memfd_create(label, os.MFD_CLOEXEC | os.MFD_ALLOW_SEALING)
    try:
        view = memoryview(data)
        while view:
            written = os.write(fd, view)
            view = view[written:]
        fcntl.fcntl(fd, fcntl.F_ADD_SEALS,
                    fcntl.F_SEAL_SEAL | fcntl.F_SEAL_SHRINK | fcntl.F_SEAL_GROW | fcntl.F_SEAL_WRITE)
        os.lseek(fd, 0, os.SEEK_SET)
        return fd
    except BaseException:
        os.close(fd)
        raise


def _namespace_directories(manifest: dict) -> list[str]:
    needed = {"/proc"}
    paths = [row["destination"] for row in manifest["inputs"]]
    paths += [manifest["execution"]["cwd"]]
    outputs = set(manifest["execution"]["private_outputs"])
    needed.update(outputs)
    paths += list(outputs)
    for value in paths:
        current = PurePosixPath(value).parent
        while str(current) != "/":
            needed.add(str(current))
            current = current.parent
    return sorted(needed, key=lambda path: (path.count("/"), path))


class PreparedBundle:
    def __init__(self, command, env, fds):
        self.command = command
        self.env = env
        self.fds = tuple(fds)
        self.closed = False

    def close(self):
        if not self.closed:
            for fd in self.fds:
                try:
                    os.close(fd)
                except OSError as exc:
                    if exc.errno != errno.EBADF:
                        raise
            self.closed = True

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        self.close()

    def run(self, timeout_seconds: float = 5.0) -> subprocess.CompletedProcess:
        if self.closed:
            raise RunnerError("prepared bundle is closed")
        if not isinstance(timeout_seconds, (int, float)) or not 0 < timeout_seconds <= 5:
            raise RunnerError("timeout must be greater than zero and at most five seconds")
        try:
            return subprocess.run(
                self.command,
                env={},
                stdin=subprocess.DEVNULL,
                close_fds=True,
                pass_fds=self.fds,
                timeout=timeout_seconds,
                check=False,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
            )
        except subprocess.TimeoutExpired as exc:
            raise BundleTimeout(f"bundle exceeded {timeout_seconds:g}s timeout") from exc


def prepare_bundle(manifest_path, bundle_root, expected_manifest_sha256) -> PreparedBundle:
    """Validate, snapshot bytes into sealed memfds, then construct bwrap argv."""
    manifest = validate_bundle(manifest_path, bundle_root, expected_manifest_sha256)
    root = Path(bundle_root).resolve(strict=True)
    bwrap = shutil.which("bwrap")
    if not bwrap:
        raise RunnerError("bwrap is not available")
    bwrap = str(Path(bwrap).resolve(strict=True))
    fds = []
    command = [bwrap, "--unshare-all", "--die-with-parent", "--new-session", "--tmpfs", "/"]
    try:
        for directory in _namespace_directories(manifest):
            command += ["--dir", directory]
        # A new procfs exposes only this PID namespace and supports fd-isolation
        # checks. It is not backed by or bound from the host filesystem.
        command += ["--proc", "/proc"]
        for row in manifest["inputs"]:
            destination = row["destination"]
            if row["type"] == "file":
                data = _read_file_nofollow(root, row["source"], row["mode"], row["sha256"])
                fd = _sealed_memfd("sealed-input", data)
                fds.append(fd)
                command += ["--perms", row["mode"], "--file", str(fd), destination]
        for row in manifest["inputs"]:
            if row["type"] == "symlink":
                command += ["--symlink", row["target"], row["destination"]]
        command += ["--remount-ro", "/proc", "--remount-ro", "/"]
        for output in manifest["execution"]["private_outputs"]:
            command += ["--tmpfs", output]
        command += ["--clearenv"]
        for key, value in sorted(manifest["execution"]["env"].items()):
            command += ["--setenv", key, value]
        command += ["--chdir", manifest["execution"]["cwd"], "--", *manifest["execution"]["argv"]]
        return PreparedBundle(command, manifest["execution"]["env"], fds)
    except BaseException:
        for fd in fds:
            os.close(fd)
        raise


def run_bundle(manifest_path, bundle_root, expected_manifest_sha256, timeout_seconds: float = 5.0):
    with prepare_bundle(manifest_path, bundle_root, expected_manifest_sha256) as prepared:
        return prepared.run(timeout_seconds)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("manifest")
    parser.add_argument("bundle_root")
    parser.add_argument("--expected-sha256", required=True)
    parser.add_argument("--timeout-seconds", type=float, default=5.0)
    args = parser.parse_args(argv)
    try:
        result = run_bundle(args.manifest, args.bundle_root, args.expected_sha256, args.timeout_seconds)
    except (BundleError, RunnerError) as exc:
        print(str(exc), file=sys.stderr)
        return 2
    sys.stdout.buffer.write(result.stdout)
    sys.stderr.buffer.write(result.stderr)
    return result.returncode


if __name__ == "__main__":
    raise SystemExit(main())
