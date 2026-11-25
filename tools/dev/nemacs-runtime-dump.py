#!/usr/bin/env python3
"""Bake the flat nemacs runtime image using cold-loadable checkpoints.

The generated bootstrap is one Lisp load. Keep its source order, lexical
scope, and load-history handling intact. Checkpoints sit between that load,
the surrounding preload work, and the installation of the cold-boot hook.
--force-full runs the continuous bake and bypasses the checkpoint cache.
"""

import argparse
import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess
import tempfile
import time


SOURCE_DIRS = ("src", "packages", "vendor", "lisp", "scripts", "standalone-compat",
               "etc", "share")
FORMAT = 1  # Bump when checkpoint execution or input semantics change.
STAGE_NAMES = ("prelude", "bootstrap", "image-preload", "reinit")
ENTRY = re.compile(r"[0-9a-f]{64}")
DONE = "NEMACS-RUNTIME-DUMP-WRITTEN"


def file_hash(path):
    with path.open("rb") as source:
        return hashlib.file_digest(source, "sha256").hexdigest()


def digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True).encode()).hexdigest()


def fingerprint(path, exclude, parents=()):
    """Hash contents and lookup names, following source-tree symlinks."""
    if path in exclude:
        return None
    if path.is_symlink():
        link = os.readlink(path)
    else:
        link = None
    try:
        info = path.stat()
    except FileNotFoundError:
        return ["missing", link]
    mode = stat.S_IMODE(info.st_mode)
    if stat.S_ISREG(info.st_mode):
        return [mode, link, file_hash(path)]
    if stat.S_ISDIR(info.st_mode):
        identity = (info.st_dev, info.st_ino)
        if identity not in parents:
            keys = ((p.name, fingerprint(p, exclude, (*parents, identity)))
                    for p in sorted(path.iterdir()))
            return [mode, link, [(name, value) for name, value in keys if value is not None]]
    return [info.st_mode, link]


def lisp_string(value):
    # NeLisp reads UTF-8 and ordinary Lisp escapes. JSON's \\u escape is
    # also understood, but keep paths in their original spelling.
    return json.dumps(str(value), ensure_ascii=False)


def load(path):
    return f"(load {lisp_string(path)} nil 'no-message t t)"


def run_lisp(args, env, form, cold=None, capture=False):
    command = ["timeout", "--kill-after=5", args.timeout, str(args.binary)]
    if cold is not None:
        command.extend(("--cold-load-from", str(cold)))
    command.extend(("--eval", form))
    return subprocess.run(command, cwd=args.root, env=env, stdin=subprocess.DEVNULL,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE if capture else None,
                          check=True)


def startup_paths(args, env):
    # The build checkout can differ from NELISP_HOME and the bake checkout.
    # Fingerprint the baked-in load path too when replay can fall back to it.
    result = run_lisp(
        args, env, "(progn (dolist (dir (cons (and (boundp 'data-directory) data-directory) load-path)) "
        '(when (stringp dir) (princ "BAKE-PATH:") (prin1 dir) (terpri))) '
        '(princ "BAKE-PATHS-END") (kill-emacs 0))', capture=True)
    lines = result.stdout.decode().splitlines()
    if not lines or lines[-1] != "BAKE-PATHS-END":
        raise ValueError("could not read standalone load-path")
    return [args.root / json.loads(line.removeprefix("BAKE-PATH:"))
            for line in lines if line.startswith("BAKE-PATH:")]


def source_roots(args, paths):
    # Avoid rereading nested load-path directories already covered by a root.
    candidates = {args.root / name for name in SOURCE_DIRS}
    candidates.update(Path(args.home) / name for name in SOURCE_DIRS)
    candidates.update(path for path in paths if path not in (args.root, Path("/")))
    roots = []
    for path in sorted(candidates, key=lambda p: (len(p.parts), str(p))):
        if not any(path.is_relative_to(parent) for parent in roots):
            roots.append(path)
    return roots


def keys(args, env, paths, stages):
    # The prelude/setup stage loads the three explicit registration scripts.
    # The generated bundle also has nested loads and fallback providers, so
    # conservatively key their entire search trees before the first such load.
    # The absolute file names are observable through load-history.
    excludes = {args.root / "scripts/nemacs-runtime-dump-reinit.el",
                args.reinit, args.cache, args.output}
    companions = [Path(str(args.binary) + suffix)
                  for suffix in ("", ".cold", ".artifact.cold")]
    key = digest([str(args.root), sorted(env.items()), FORMAT,
                  [(str(p), fingerprint(p, set()))
                   for p in companions + args.preload + args.input]])
    stage_inputs = [
        None,
        [(str(path), fingerprint(path, excludes))
         for path in [args.bundle, *source_roots(args, paths)]],
        None,  # All live source roots are already in the bootstrap key.
        [(str(p), fingerprint(p, set())) for p in [args.reinit, *args.profile]],
    ]
    chain = []
    for forms, inputs in zip(stages, stage_inputs, strict=True):
        key = digest([key, forms, inputs])
        chain.append(key)
    return chain


def write_dump(args, env, forms, target, cold=None, checkpoint=False):
    start = time.monotonic()
    # The production wrapper warms the final image. Use its raw dumper
    # for checkpoints so a boundary cannot compile functions ahead of
    # their normal first call in the next stage.
    dumper = ("(if (fboundp 'nelisp--fx-plain-dump-image-stream) "
              "#'nelisp--fx-plain-dump-image-stream #'nelisp--arena-dump-image-stream)"
              if checkpoint else "#'nelisp--arena-dump-image-stream")
    form = ("(progn " + " ".join(forms)
            + f" (unless (> (funcall {dumper} {lisp_string(target)}) 0)"
              ' (error "could not write flat runtime dump"))'
            + f' (princ "{DONE}\\n") (kill-emacs 0))')
    result = run_lisp(args, env, form, cold)
    for line in result.stdout.splitlines():
        if line != DONE.encode():
            print(line.decode(errors="replace"), flush=True)
    if DONE.encode() not in result.stdout.splitlines() or not target.stat().st_size:
        raise ValueError(f"incomplete flat dump at {target}")
    print(f"baked in {time.monotonic() - start:.2f}s", flush=True)


def cache_entries(cache):
    return (p for p in cache.iterdir() if ENTRY.fullmatch(p.name) and p.is_dir())


def valid_entry(entry):
    try:
        image = entry / "image"
        record = json.loads((entry / "receipt").read_text())
        return (record["size"] == image.stat().st_size
                and record["size"] > 0 and record["sha256"] == file_hash(image))
    except (OSError, ValueError, KeyError, TypeError):
        return False


def trim_cache(cache, budget, keep, active=None):
    entries = sorted(cache_entries(cache),
                     key=lambda p: (p == active, p in keep, p.stat().st_mtime_ns, p.name),
                     reverse=True)
    for entry in entries:
        image = entry / "image"
        receipt = entry / "receipt"
        size = (image.stat().st_size + receipt.stat().st_size
                if image.is_file() and receipt.is_file() else 0)
        if size and size <= budget:
            budget -= size
        else:
            shutil.rmtree(entry)


def atomic_output(source, output):
    if (output.is_file() and output.stat().st_size == source.stat().st_size
            and file_hash(output) == file_hash(source)):
        return
    # The output has independent lifetime, even if immediately pruned.
    with tempfile.TemporaryDirectory(prefix=".runtime-dump-", dir=output.parent) as work:
        candidate = Path(work) / "image"
        shutil.copyfile(source, candidate)
        os.replace(candidate, output)


def bake_incremental(args, env, stages):
    args.cache.mkdir(parents=True, exist_ok=True)
    with (args.cache / ".lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        # Remove leftovers from interrupted writers under the same lock.
        for path in args.cache.glob(".pending-*"):
            shutil.rmtree(path)
        paths = startup_paths(args, env)
        chain = keys(args, env, paths, stages)
        entries = [args.cache / key for key in chain]
        keep = set(entries)
        cold = None
        start = 0
        for index in reversed(range(len(entries))):
            entry = entries[index]
            if valid_entry(entry):
                os.utime(entry)
                cold = entry / "image"
                start = index + 1
                break
        prefix = STAGE_NAMES[start - 1] if start else "none"
        print(f"resumed {prefix} ({start}/{len(stages)})", flush=True)
        # A private workspace holds at most the currently produced checkpoint
        # and, if it exceeds the budget, its predecessor. No reader can see
        # either until the receipt and image are atomically renamed together.
        with tempfile.TemporaryDirectory(prefix=".pending-", dir=args.cache) as work:
            for index in range(start, len(stages)):
                print(f"stage {STAGE_NAMES[index]} {chain[index]}", flush=True)
                pending = Path(tempfile.mkdtemp(dir=work))
                image = pending / "image"
                write_dump(args, env, stages[index], image, cold,
                           checkpoint=index < len(stages) - 1)
                if keys(args, env, paths, stages) != chain:
                    raise ValueError("bake inputs changed during replay")
                if cold is not None and cold.is_relative_to(work):
                    shutil.rmtree(cold.parent)
                cold = image
                size = image.stat().st_size
                if size < args.cache_bytes:
                    receipt = pending / "receipt"
                    receipt.write_text(json.dumps(
                        {"size": size, "sha256": file_hash(image)}) + "\n")
                    if size + receipt.stat().st_size <= args.cache_bytes:
                        entry = entries[index]
                        if entry.exists():
                            shutil.rmtree(entry)
                        pending.rename(entry)
                        cold = entry / "image"
                        trim_cache(args.cache, args.cache_bytes, keep, active=entry)
            if keys(args, env, paths, stages) != chain:
                raise ValueError("bake inputs changed before publication")
            atomic_output(cold, args.output)
        trim_cache(args.cache, args.cache_bytes, keep)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument("--home", type=Path)
    parser.add_argument("--binary", type=Path, default=Path("target/nelisp"))
    parser.add_argument("--bundle", type=Path, default=Path("build/nemacs-bootstrap.el"))
    parser.add_argument("--output", type=Path, default=Path("build/nemacs-runtime.dump"))
    parser.add_argument("--cache", type=Path, default=Path("build/runtime-dump-cache"))
    parser.add_argument("--cache-bytes", type=int, default=2 * 1024**3)
    parser.add_argument("--timeout", default="1800s", help="deadline per NeLisp process")
    parser.add_argument("--preload", type=Path, nargs=3, default=[
        Path(f"scripts/nemacs-runtime-{name}-preload.el")
        for name in ("process", "frame-tab", "image")])
    parser.add_argument("--reinit", type=Path, default=Path("scripts/nemacs-runtime-dump-reinit.el"))
    parser.add_argument("--input", type=Path, action="append", default=[],
                        help="additional ambient input file or directory (repeatable)")
    parser.add_argument("--force-full", action="store_true")
    args = parser.parse_args()
    args.root = args.root.resolve()

    def absolute(path):
        return Path(os.path.abspath(args.root / path))

    for name in ("binary", "bundle", "output", "cache", "reinit"):
        setattr(args, name, absolute(getattr(args, name)))
    args.preload = [absolute(path) for path in args.preload]
    args.input = [absolute(path) for path in args.input]
    args.home = absolute(args.home or os.environ.get("NELISP_HOME") or args.root)
    if args.cache_bytes < 0:
        parser.error("--cache-bytes must be nonnegative")
    if args.output.resolve().is_relative_to(args.cache.resolve()):
        parser.error("--output must be outside --cache")

    # Transport IDs and shell bookkeeping vary between otherwise identical
    # invocations. Keep them out of both the child environment and its key.
    transport = ("CODEX_", "CLAUDE_CODE_", "BLE_", "_ble_", "BASH_FUNC_",
                 "NEMACS_RUNTIME_DUMP_")
    housekeeping = {"MAKEFLAGS", "MFLAGS", "MAKEOVERRIDES", "MAKELEVEL",
                    "SHLVL", "_", "PWD", "OLDPWD", "LINENO", "SHELLOPTS", "BASHOPTS",
                    "NEMACS_RUNTIME_DUMP", "NEMACS_RUNTIME_BAKE_TIMEOUT"}
    env = {name: value for name, value in os.environ.items()
           if name not in housekeeping and not name.startswith(transport)}
    env["NELISP_HOME"] = str(args.home)
    args.profile = ([absolute(Path(env["NELISP_FX_PROFILE"]).expanduser())]
                    if env.get("NELISP_FX_PROFILE") else [])

    root, bundle = lisp_string(args.root), lisp_string(args.bundle)
    preload = [load(path) for path in args.preload]
    stages = [
        preload + [f"(nemacs-runtime-image-setup-paths {root})"],
        [f"(nemacs-runtime-image-load-bootstrap {root} {bundle})"],
        [f"(nemacs-runtime-image-finish-batch-preload {root})"],
        [load(args.reinit)],
    ]
    start = time.monotonic()
    args.output.parent.mkdir(parents=True, exist_ok=True)
    try:
        if args.force_full:
            with tempfile.TemporaryDirectory(prefix=".runtime-dump-", dir=args.output.parent) as work:
                image = Path(work) / "image"
                write_dump(args, env, preload + [
                    f"(nemacs-runtime-image-preload-batch {root} {bundle})",
                    load(args.reinit)], image)
                os.replace(image, args.output)
        else:
            bake_incremental(args, env, stages)
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        parser.exit(1, f"runtime dump bake: {error}\n")
    print(f"runtime dump {args.output} ({time.monotonic() - start:.2f}s)", flush=True)


if __name__ == "__main__":
    main()
