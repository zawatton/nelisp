#!/usr/bin/env python3
"""Build a private NeLisp user heap and select it before ordinary startup."""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import stat
import subprocess
import sys
import tempfile
import time
import uuid

ROOT = Path(__file__).resolve().parents[2]
PRELOAD = Path(__file__).resolve().with_suffix(".el")
FORMAT = 1
MANIFEST = "manifest.json"
HEAP_NAME = re.compile(r"heap-[a-f0-9]{32}\.dump\Z")
LOCAL_FLAGS = os.O_RDONLY | os.O_NONBLOCK
PRIVATE_FLAGS = LOCAL_FLAGS | os.O_NOFOLLOW
INPUT_RECORD = b"NEMACS-USER-SNAPSHOT-INPUTS "
SUCCESS = b"NEMACS-USER-SNAPSHOT-WRITTEN"


class InvalidSnapshot(Exception):
    """The preload or a recorded input cannot be used."""


def absolute(path):
    # Keep the spelled path: retargeting a symlink must invalidate its record.
    return os.path.abspath(os.path.expanduser(path))


def identity(info):
    return [info.st_dev, info.st_ino, info.st_mode, info.st_size,
            info.st_mtime_ns, info.st_ctime_ns]


def fingerprint(path, *, content=False):
    """Record absence as well as local path identity and optional content."""
    try:
        info = os.stat(path)
    except (FileNotFoundError, NotADirectoryError):
        return None
    if not content:
        return identity(info)
    target = os.path.realpath(path)
    record = [target, *identity(info)]
    if stat.S_ISREG(info.st_mode):
        with os.fdopen(os.open(path, LOCAL_FLAGS), "rb") as source:
            record.append(digest(source))
            if identity(os.fstat(source.fileno())) != identity(info):
                raise InvalidSnapshot(f"input changed while reading {path}")
        if identity(os.stat(path)) != identity(info) or os.path.realpath(path) != target:
            raise InvalidSnapshot(f"input changed while reading {path}")
    return record


def digest(source):
    sha = hashlib.sha256()
    while block := source.read(1024 * 1024):
        sha.update(block)
    return sha.hexdigest()


def open_private(directory, name, kind):
    descriptor = os.open(name, PRIVATE_FLAGS, dir_fd=directory)
    info = os.fstat(descriptor)
    try:
        if not kind(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
            raise InvalidSnapshot(f"expected private user-owned state at {name}")
        return descriptor
    except BaseException:
        os.close(descriptor)
        raise


def check_inputs(records, *, content):
    for path, recorded in records.items():
        if fingerprint(path, content=content) != recorded:
            raise InvalidSnapshot(f"changed input {path}")


def stable_inputs(args):
    init_names = ("early-init.el", "init.el")
    variants = ("", ".gz", "c", "c.gz")
    init_files = (os.path.join(args.init_directory, name + suffix)
                  for name in init_names for suffix in variants)
    return dict.fromkeys([args.binary, args.base, str(PRELOAD), __file__,
                          *init_files, *args.input])


def lisp_time_ns(value):
    high, low, microseconds, picoseconds = value
    return ((high << 16) + low) * 10**9 + microseconds * 1000 + picoseconds // 1000


def collect_inputs(observations, fixed):
    base_path, base_history, reads, lookups, environment, directories, added_paths = observations
    inputs = dict(fixed)
    for path in base_history:
        if os.path.isabs(path) and path not in inputs:
            inputs[path] = fingerprint(path, content=True)
    for path, expected in reads:
        info = fingerprint(path, content=True)
        if info is None or info[-1] != expected:
            raise InvalidSnapshot(f"loaded file changed during bake: {path}")
        if path in inputs and info != inputs[path]:
            raise InvalidSnapshot(f"input changed during bake: {path}")
        inputs[path] = info

    candidates = {}
    for name, suffixes, path, is_absolute in lookups:
        for directory in path:
            base = directory if is_absolute else os.path.join(directory, name)
            for suffix in suffixes:
                candidate = os.path.abspath(base + suffix)
                if candidate not in inputs and candidate not in candidates:
                    candidates[candidate] = fingerprint(candidate)

    for path, stamp in directories:
        try:
            info = os.stat(path)
        except (FileNotFoundError, NotADirectoryError):
            current = None
        else:
            current = [info.st_mtime_ns, info.st_ctime_ns, [info.st_ino, info.st_dev]]
        if stamp is not None:
            stamp = [lisp_time_ns(stamp[0]), lisp_time_ns(stamp[1]), stamp[2]]
        if current != stamp:
            raise InvalidSnapshot(f"search directory changed during bake: {path}")

    # Bind the observed values to the live process environment. An
    # environment mutated by a package is outside this preload contract.
    for name, expected in environment:
        if os.environ.get(name) != expected:
            raise InvalidSnapshot(f"preload used a modified environment variable: {name}")
    environment = dict(environment)
    environment["EMACSLOADPATH"] = os.environ.get("EMACSLOADPATH")
    environment["NELISP_HOME"] = os.environ.get("NELISP_HOME")
    return dict(inputs=inputs, candidates=candidates, base_load_path=base_path,
                package_load_path=added_paths, environment=environment, relative_path=any(
                    path is None or not os.path.isabs(path) for path in base_path))


def lisp_string(value):
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def bake(args):
    start = time.monotonic()
    directory = Path(args.snapshot)
    directory.mkdir(mode=0o700, parents=True, exist_ok=True)
    os.close(open_private(None, directory, stat.S_ISDIR))
    fixed = {path: fingerprint(path, content=True) for path in stable_inputs(args)}
    for path in (args.binary, args.base):
        if fixed[path] is None or not stat.S_ISREG(fixed[path][3]):
            raise InvalidSnapshot(f"missing regular input: {path}")
    for path in args.input:
        if fixed[path] is None or not (stat.S_ISREG(fixed[path][3]) or stat.S_ISDIR(fixed[path][3])):
            raise InvalidSnapshot(f"missing file or directory input: {path}")
    for path in (args.init_directory, *args.load_path):
        if not os.path.isdir(path):
            raise InvalidSnapshot(f"missing directory: {path}")
    if os.environ.get("EMACSLOADPATH") is not None:
        raise InvalidSnapshot("bake with unset EMACSLOADPATH; use ordered --load-path options")
    if not all(re.fullmatch(r"[\w+*/:<>=!?-]+", feature) for feature in args.require):
        raise InvalidSnapshot("invalid --require feature name")

    with tempfile.TemporaryDirectory(prefix=".bake-", dir=directory) as stage:
        heap = Path(stage) / "heap.dump"
        form = f"""(progn
          (nemacs-runtime-dump-reinit)
          (let ((history load-history))
            (load {lisp_string(str(PRELOAD))} nil t t)
            (setq load-history history))
          (nemacs-user-snapshot--prepare {lisp_string(args.init_directory)}
             '( {' '.join(map(lisp_string, args.load_path))} )
             '( {' '.join(args.require)} ) {'t' if args.prefer_newer else 'nil'})
          (setq nemacs-initialized nil command-line-processed nil
                command-line-args nil command-line-args-left nil
                process-environment nil initial-environment nil
                post-gc-hook nil)
          (when (boundp 'nlre--nat-scratch) (setq nlre--nat-scratch nil))
          (garbage-collect)
          (unless (> (nelisp--arena-dump-image-stream {lisp_string(str(heap))}) 0)
            (error "User snapshot writer failed"))
          (princ "{SUCCESS.decode()}")
          (terpri))"""
        result = subprocess.run([args.binary, "--cold-load-from", args.base, "--eval", form],
                                stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, timeout=args.timeout,
                                umask=0o077)
        records = [line[len(INPUT_RECORD):] for line in result.stdout.splitlines()
                   if line.startswith(INPUT_RECORD)]
        if result.returncode or len(records) != 1 or SUCCESS not in result.stdout.splitlines():
            diagnostics = result.stderr or b"\n".join(
                line for line in result.stdout.splitlines() if not line.startswith(INPUT_RECORD))
            raise InvalidSnapshot("preload failed: " + diagnostics.decode(errors="replace")[-4000:])
        data = json.loads(records[0])
        collected = collect_inputs(data, fixed)
        check_inputs(collected["inputs"], content=True)
        check_inputs(collected["candidates"], content=False)
        with heap.open("rb") as source:
            heap_digest = digest(source)
            os.fsync(source.fileno())
        size = heap.stat().st_size
        if not size:
            raise InvalidSnapshot("empty user heap")
        heap.chmod(0o400)
        name = f"heap-{uuid.uuid4().hex}.dump"
        manifest = dict(version=FORMAT, heap=name, digest=heap_digest, base=args.base,
                        binary=args.binary, profile=args.init_directory,
                        load_path=args.load_path, features=args.require,
                        cwd=os.getcwd(), prefer_newer=args.prefer_newer, **collected)
        staged_manifest = Path(stage) / MANIFEST
        with staged_manifest.open("x", encoding="utf-8") as output:
            os.fchmod(output.fileno(), 0o600)
            json.dump(manifest, output, ensure_ascii=True, sort_keys=True)
            output.write("\n")
            output.flush()
            os.fsync(output.fileno())
        # Publish the manifest last. Older immutable heaps remain usable by
        # concurrent readers who already opened their manifest.
        heap.rename(directory / name)
        staged_manifest.replace(directory / MANIFEST)
        directory_fd = os.open(directory, os.O_RDONLY | os.O_DIRECTORY)
        try:
            os.fsync(directory_fd)
        finally:
            os.close(directory_fd)
        print(f"{directory / name}\t{size} bytes\t{time.monotonic() - start:.3f} s")


def open_snapshot(args):
    directory = open_private(None, args.snapshot, stat.S_ISDIR)
    try:
        with os.fdopen(open_private(directory, MANIFEST, stat.S_ISREG), "rb") as source:
            manifest = json.load(source)
        if manifest["version"] != FORMAT:
            raise InvalidSnapshot("manifest version changed")
        if (manifest["binary"], manifest["base"], manifest["profile"]) != (
                args.binary, args.base, args.init_directory):
            raise InvalidSnapshot("executable, base or init directory changed")
        if manifest["relative_path"] and manifest["cwd"] != os.getcwd():
            raise InvalidSnapshot("relative load-path used in another directory")
        for name, expected in manifest["environment"].items():
            if os.environ.get(name) != expected:
                raise InvalidSnapshot(f"changed environment variable {name}")
        check_inputs(manifest["inputs"], content=True)
        check_inputs(manifest["candidates"], content=False)
        name = manifest["heap"]
        if not HEAP_NAME.fullmatch(name):
            raise InvalidSnapshot("invalid heap filename")
        heap = open_private(directory, name, stat.S_ISREG)
        # Hash and open the same inode the runtime will map. The private
        # immutable generation can be unlinked after this point.
        try:
            with os.fdopen(os.dup(heap), "rb") as source:
                if digest(source) != manifest["digest"]:
                    raise InvalidSnapshot("heap digest mismatch")
        except BaseException:
            os.close(heap)
            raise
        try:
            binary = os.open(args.binary, LOCAL_FLAGS)
            if identity(os.fstat(binary)) != manifest["inputs"][args.binary][1:7]:
                os.close(binary)
                raise InvalidSnapshot("executable changed during validation")
        except BaseException:
            os.close(heap)
            raise
        return heap, binary
    finally:
        os.close(directory)


def run(args):
    arguments = args.arguments
    if arguments[:1] == ["--"]:
        arguments = arguments[1:]
    if len(arguments) != 2 or arguments[0] != "--eval":
        raise InvalidSnapshot("snapshot startup requires one --eval entry form")
    heap = args.base
    executable = args.binary
    try:
        descriptor, executable = open_snapshot(args)
    except (InvalidSnapshot, OSError, ValueError, KeyError, TypeError, AttributeError) as error:
        print(f"nemacs: user snapshot unavailable ({error}); starting from base", file=sys.stderr)
    else:
        os.set_inheritable(descriptor, True)
        heap = f"/proc/self/fd/{descriptor}"
        # The arena is mapped before its first Lisp form. Drop the inherited
        # ownership descriptor before startup hooks or package subprocesses.
        arguments[1] = f"(progn (syscall-direct 3 {descriptor} 0 0 0 0 0) {arguments[1]})"
    os.execve(executable, [args.binary, "--cold-load-from", heap, *arguments], os.environ)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    subcommands = parser.add_subparsers(required=True)
    common = argparse.ArgumentParser(add_help=False)
    common.add_argument("--snapshot", type=absolute, required=True)
    common.add_argument("--binary", type=absolute, default=str(ROOT / "target/nelisp"))
    common.add_argument("--base", type=absolute, default=str(ROOT / "build/nemacs-runtime.dump"))
    common.add_argument("--init-directory", type=absolute, required=True)

    producer = subcommands.add_parser("bake", parents=[common])
    producer.add_argument("--require", action="append", required=True)
    producer.add_argument("--load-path", type=absolute, action="append", default=[])
    producer.add_argument("--input", type=absolute, action="append", default=[])
    producer.add_argument("--prefer-newer", action="store_true")
    producer.add_argument("--timeout", type=float, default=300)
    producer.set_defaults(function=bake)

    launcher = subcommands.add_parser("run", parents=[common])
    launcher.add_argument("arguments", nargs=argparse.REMAINDER)
    launcher.set_defaults(function=run)
    args = parser.parse_args()
    os.environ.setdefault("NELISP_HOME", str(ROOT))
    if sys.platform != "linux" or os.uname().machine != "x86_64":
        parser.exit(1, "nemacs: user snapshots require Linux x86-64\n")
    try:
        args.function(args)
    except (InvalidSnapshot, OSError, ValueError, subprocess.TimeoutExpired) as error:
        parser.exit(1, f"nemacs: {error}\n")


if __name__ == "__main__":
    main()
