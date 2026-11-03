#!/usr/bin/env python3
"""Run one pinned native application producer/harvest transaction in an owned child.

This is an orchestration adapter. It neither parses application forms nor admits
native code; host-side validators remain responsible for acceptance.
"""
from __future__ import annotations

import hashlib
from contextlib import contextmanager
import importlib.util
import json
import os
import re
from pathlib import Path
import resource
import secrets
import shutil
import signal
import stat
import sys
import tempfile


ROLES = {
    "repository", "producer_image", "cold_image", "build_manifest", "producer",
    "helper", "prelude", "harvester", "launcher", "guard", "owned_runner",
    "names", "application_manifest",
}
ENV_PREFIXES = ("NELISP_", "V8_", "FX_", "EMACS")


class RequestError(ValueError):
    pass


def _need(ok: bool, message: str) -> None:
    if not ok:
        raise RequestError(message)


def _sha(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest()


def _tree_sha(path: Path) -> str:
    """Hash a source tree by relative path and file content, rejecting links."""
    h = hashlib.sha256()
    for item in sorted(path.rglob("*")):
        _need(not item.is_symlink(), f"repository tree contains symlink: {item.relative_to(path)}")
        _need(item.is_file() or item.is_dir(), "repository tree contains a special file")
        if item.is_file():
            rel = item.relative_to(path).as_posix().encode("utf-8")
            h.update(len(rel).to_bytes(8, "big")); h.update(rel)
            h.update(bytes.fromhex(_sha(item)))
    return h.hexdigest()


def _pinned_file(role: str, pin: dict) -> Path:
    _need(isinstance(pin, dict) and set(pin) == {"path", "sha256"}, f"bad pin: {role}")
    _need(isinstance(pin["path"], str) and isinstance(pin["sha256"], str) and
          re.fullmatch(r"[0-9a-f]{64}", pin["sha256"]) is not None, f"bad pin types: {role}")
    path = Path(pin["path"])
    _need(path.is_absolute(), f"pin path must be absolute: {role}")
    _need(not any(part.is_symlink() for part in (path, *path.parents)),
          f"pin path must not traverse a symlink: {role}")
    if role == "repository":
        _need(path.is_dir(), "repository pin must name a directory")
        actual = _tree_sha(path)
    else:
        _need(path.is_file(), f"pin must be a regular file: {role}")
        actual = _sha(path)
    _need(len(pin["sha256"]) == 64 and actual == pin["sha256"], f"pin hash mismatch: {role}")
    return path.resolve(strict=True)


def _lisp_string(value: str) -> str:
    """Return a Lisp string literal; request data is never inserted as Lisp code."""
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n") + '"'


def _load_module(path: Path, name: str):
    spec = importlib.util.spec_from_file_location(name, path)
    _need(spec is not None and spec.loader is not None, f"cannot load pinned module: {name}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _json(raw: bytes, label: str):
    """Decode strict JSON objects without duplicate-key or nonstandard constants."""
    def pairs(items):
        result = {}
        for key, value in items:
            _need(key not in result, f"duplicate JSON key in {label}: {key}")
            result[key] = value
        return result

    def constant(value):
        raise RequestError(f"nonstandard JSON constant in {label}: {value}")

    return json.loads(raw.decode("utf-8"), object_pairs_hook=pairs, parse_constant=constant)


@contextmanager
def _owned_file(out: Path, path: Path, label: str):
    """Open only a private regular artifact, never a symlink or shared inode."""
    _need(path.is_relative_to(out), f"artifact outside owned output: {label}")
    for directory in (path.parent, *path.parent.parents):
        info = directory.lstat()
        _need(stat.S_ISDIR(info.st_mode) and info.st_uid == os.getuid(),
              f"artifact directory is not owned: {label}")
        if directory == out:
            break
    try:
        descriptor = os.open(path, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
    except OSError as exc:
        raise RequestError(f"owned regular artifact missing or unsafe: {label}") from exc
    with os.fdopen(descriptor, "rb") as stream:
        before = os.fstat(stream.fileno())
        _need(stat.S_ISREG(before.st_mode) and before.st_uid == os.getuid() and before.st_nlink == 1,
              f"artifact must be an independent owned regular file: {label}")
        yield stream
        after = path.lstat()
        _need(stat.S_ISREG(after.st_mode) and after.st_nlink == 1 and
              (before.st_dev, before.st_ino) == (after.st_dev, after.st_ino),
              f"artifact identity changed: {label}")


def _artifact_bytes(out: Path, path: Path, label: str) -> bytes:
    with _owned_file(out, path, label) as stream:
        return stream.read()


def _artifact_sha(out: Path, path: Path) -> str:
    with _owned_file(out, path, path.name) as stream:
        digest = hashlib.sha256()
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
        return digest.hexdigest()


def _write_capture(out: Path, evidence: dict, *, rejected=False) -> Path:
    """Create evidence exclusively; retain child-created aliases without following them."""
    directory = os.open(out, os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW)
    try:
        _need(os.fstat(directory).st_uid == os.getuid(), "capture directory is not owned")
        name = "capture.json"
        while True:
            try:
                descriptor = os.open(name, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
                                     0o600, dir_fd=directory)
                break
            except FileExistsError:
                _need(rejected, "capture output already exists")
                name = "capture-rejected-" + secrets.token_hex(8) + ".json"
    finally:
        os.close(directory)
    with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
        stream.write(json.dumps(evidence, indent=2, sort_keys=True) + "\n")
    return out / name


def _request(path: Path) -> tuple[dict, dict[str, Path], list[str]]:
    _need(not any(part.is_symlink() for part in (path, *path.parents)) and path.is_file(),
          "request must be a nonsymlink regular file")
    request = _json(path.read_bytes(), "request")
    required = {"schema", "pins", "mode", "names_sha256", "id0", "constant", "output_parent"}
    _need(isinstance(request, dict) and set(request) == required, "request fields do not match schema")
    _need(request["schema"] == "nelisp-native-application-harvest/v1", "unsupported request schema")
    _need(request["mode"] in ("strict", "selected-definitions"), "invalid application mode")
    _need(type(request["id0"]) is int and request["id0"] >= 0, "invalid initial row id")
    _need(isinstance(request["constant"], str) and
          re.fullmatch(r"[A-Za-z_][A-Za-z0-9_-]*", request["constant"]) is not None,
          "invalid data constant")
    pins_obj = request["pins"]
    _need(isinstance(pins_obj, dict) and set(pins_obj) == ROLES, "pin roles do not match schema")
    pins = {role: _pinned_file(role, pin) for role, pin in pins_obj.items()}
    names_raw = pins["names"].read_bytes()
    _need(hashlib.sha256(names_raw).hexdigest() == request["names_sha256"], "names hash mismatch")
    names = names_raw.decode("utf-8").splitlines()
    _need(names and all(re.fullmatch(r"[A-Za-z_][A-Za-z0-9_-]*", n) for n in names), "invalid names input")
    _need(len(names) == len(set(names)), "duplicate names input")
    # application_manifest is the existing pinned Lisp plist, not request JSON.
    # Only the real producer/Lisp validators interpret its names and source pins.
    return request, pins, names


def _safe_environment(pins: dict[str, Path], names: Path, out: Path, request: dict) -> dict[str, str]:
    env = {"LC_ALL": "C", "PATH": os.environ.get("PATH", "/usr/bin:/bin")}
    env.update({
        "NELISP_PRODUCER_IMAGE_SHA256": _sha(pins["producer_image"]),
        "NELISP_FXIR_ROOT": str(pins["repository"]),
        "NC1_NAMES": str(names),
        "NC1_OUT": str(out / "rows.el"),
        "NC1_ID0": str(request["id0"]),
        "NC1_CONST": request["constant"],
        "NELISP_APPLICATION_ROOT": str(pins["repository"]),
        "NELISP_APPLICATION_MANIFEST": str(pins["application_manifest"]),
        "NELISP_APPLICATION_MANIFEST_SHA256": _sha(pins["application_manifest"]),
        "NELISP_APPLICATION_NAMES_SHA256": request["names_sha256"],
        "NELISP_APPLICATION_MODE": request["mode"],
        "NELISP_APPLICATION_INSTALL_RECEIPT": str(out / "install-receipt.el"),
    })
    return env


def _driver(pins: dict[str, Path], receipt: Path) -> str:
    """Build fixed driver text. Paths are serialized as Lisp string data."""
    root = _lisp_string(str(pins["repository"]))
    producer = _lisp_string(str(pins["producer"]))
    helper = _lisp_string(str(pins["helper"]))
    prelude = _lisp_string(str(pins["prelude"]))
    harvester = _lisp_string(str(pins["harvester"]))
    receipt_lit = _lisp_string(str(receipt))
    return f""";;; generated fixed lifecycle driver; request values are data only
(setq default-directory {root})
(setq load-path (append (list (expand-file-name "lisp" {root}))
                        load-path))
(load {helper} nil t)
(load {prelude} nil t)
(load {producer} nil t)
(let ((receipt (nelisp-native-application-producer-install
                (getenv \"NELISP_APPLICATION_MANIFEST\")
                (getenv \"NC1_NAMES\")
                (getenv \"NELISP_APPLICATION_MANIFEST_SHA256\")
                (getenv \"NELISP_APPLICATION_NAMES_SHA256\")
                (getenv \"NELISP_PRODUCER_IMAGE_SHA256\")
                (if (equal (getenv \"NELISP_APPLICATION_MODE\") \"selected-definitions\")
                    :selected-definitions nil))))
  (with-temp-file {receipt_lit}
    (let ((print-length nil) (print-level nil)) (prin1 receipt (current-buffer)))))
(load {harvester} nil t)
"""


def _owned_run(request: dict, pins: dict[str, Path], out: Path, argv: list[str], env: dict[str, str]):
    launcher = _load_module(pins["launcher"], "nelisp_owned_launcher")
    guard = _load_module(pins["guard"], "nelisp_owned_guard")
    report = {"status": "INCOMPLETE", "children": [], "accepted": False}
    cancelled: list[int] = []
    run, runner_hash = launcher.owned_function(
        str(pins["owned_runner"]), guard, out, report, cancelled)
    _need(callable(run) and isinstance(runner_hash, str), "owned runner unavailable")
    old_as = resource.getrlimit(resource.RLIMIT_AS)
    old_core = resource.getrlimit(resource.RLIMIT_CORE)
    _need(old_as[0] != resource.RLIM_INFINITY and old_as[0] <= 8 * 1024**3,
          "address-space limit must be at most 8 GiB")
    _need(old_core[0] == 0, "core dumps must be disabled")
    old_handler = signal.signal(signal.SIGTERM, lambda sig, frame: cancelled.append(sig))
    old_environment = dict(os.environ)
    try:
        os.environ.clear()
        os.environ.update({k: v for k, v in old_environment.items()
                           if not k.startswith(ENV_PREFIXES)})
        run(argv, out / "child", env)
    finally:
        os.environ.clear()
        os.environ.update(old_environment)
        signal.signal(signal.SIGTERM, old_handler)
    _need(not cancelled, "owned child cancellation observed")
    child_dir = out / "child"
    command_path = child_dir / "command.json"
    launch_path = child_dir / "launch.json"
    command = _json(_artifact_bytes(out, command_path, "command.json"), "command")
    launch = _json(_artifact_bytes(out, launch_path, "launch.json"), "launch")
    controller = command.get("controller", {})
    child = command.get("child", {})
    _need(command.get("timeout") == 45, "owned runner deadline must be 45 seconds")
    _need(controller.get("pid") == os.getpid() and controller.get("uid") == os.getuid(),
          "owned runner controller identity mismatch")
    _need(isinstance(controller.get("start_ticks"), int) and isinstance(controller.get("session"), int),
          "owned runner controller identity incomplete")
    _need(isinstance(child.get("pid"), int) and child.get("pid") > 0 and
          child.get("session") == child.get("pid") and child.get("uid") == os.getuid() and
          isinstance(child.get("start_ticks"), int), "owned child identity mismatch")
    _need(launch.get("pid") == child.get("pid") and launch.get("owned_unreaped_child") is True,
          "owned launch identity mismatch")
    _need(command.get("cleanup", {}).get("verified") is True and
          command["cleanup"].get("scope", "").startswith("Owned SID only"),
          "owned child SID cleanup not verified")
    _need(command.get("error") is None and not command.get("timed_out") and not command.get("cancelled"),
          "owned process lifecycle failed")
    return report, runner_hash


def run_request(request_path: str | os.PathLike[str]) -> dict:
    """Validate and run one owned producer followed by terminal nc1-harvest."""
    request_arg = Path(request_path)
    _need(request_arg.is_absolute() and not any(part.is_symlink() for part in (request_arg, *request_arg.parents)),
          "request path must be absolute and must not traverse a symlink")
    request_file = request_arg.resolve(strict=True)
    request_hash = _sha(request_file)
    request, pins, names = _request(request_file)
    _need(_sha(request_file) == request_hash, "request changed during validation")
    parent_arg = Path(request["output_parent"])
    _need(parent_arg.is_absolute() and not any(part.is_symlink() for part in (parent_arg, *parent_arg.parents)),
          "output parent must be absolute and must not traverse a symlink")
    parent = parent_arg.resolve(strict=True)
    _need(parent.is_dir(), "output parent must be an existing directory")
    _need(not parent.is_relative_to(pins["repository"]), "output parent is inside pinned repository")
    # tempfile creates a new, private leaf; no caller-supplied output leaf is overwritten.
    out = Path(tempfile.mkdtemp(prefix="nc1-owned-", dir=parent))
    os.chmod(out, stat.S_IRWXU)
    evidence = {"status": "INCOMPLETE", "pins": {
                    role: {"path": request["pins"][role]["path"],
                           "sha256": _tree_sha(path) if role == "repository" else _sha(path)}
                    for role, path in pins.items()},
                "request_sha256": request_hash, "outputs": {}, "rejected": True}
    names_copy = out / "names.txt"
    driver = out / "producer-driver.el"
    try:
        for role, descriptor in evidence["pins"].items():
            _need(descriptor["sha256"] == request["pins"][role]["sha256"],
                  f"pin changed during capture setup: {role}")
        shutil.copyfile(pins["names"], names_copy)
        driver.write_text(_driver(pins, out / "install-receipt.el"), encoding="utf-8")
        generated = {path: _sha(path) for path in (names_copy, driver)}
        env = _safe_environment(pins, names_copy, out, request)
        argv = [str(pins["producer_image"]), "--load", str(driver)]
        report, runner_hash = _owned_run(request, pins, out, argv, env)
        # nc1-harvest ends with kill-emacs; only its files and the child receipt
        # determine completion after launch. No code runs in that process afterward.
        child = out / "child"
        stdout = child / "stdout"
        stderr = child / "stderr"
        receipt = out / "install-receipt.el"
        rows = out / "rows.el"
        _need(_artifact_bytes(out, child / "rc", "child rc").strip() == b"0", "producer child failed")
        _need(receipt.exists(), "producer receipt missing")
        _need(rows.exists(), "harvest rows missing")
        _need(_artifact_bytes(out, receipt, "producer receipt"), "producer receipt missing")
        _need(_artifact_bytes(out, rows, "harvest rows"), "harvest rows missing")
        _need(not _artifact_bytes(out, stderr, "child stderr"), "child logs missing or stderr nonempty")
        lifecycle = _json(_artifact_bytes(out, child / "command.json", "command.json"), "command")
        _artifact_bytes(out, child / "launch.json", "launch.json")
        # Revalidate the complete request and every explicit file/tree pin after cleanup.
        post_request, post_pins, post_names = _request(request_file)
        _need(_sha(request_file) == request_hash and post_request == request and
              post_pins == pins and post_names == names, "request changed after child cleanup")
        _need(all(_artifact_sha(out, path) == expected for path, expected in generated.items()),
              "generated driver or names changed after launch")
        # Preserve exact evidence; semantic admission/completeness remains in Lisp.
        evidence.update(status="RAW_CAPTURED_NOT_ADMITTED", rejected=False,
                        runner_ast_sha256=runner_hash, owned_report=report,
                        child_lifecycle=lifecycle,
                        outputs={p.name: _artifact_sha(out, p)
                                 for p in (stdout, stderr, receipt, rows, driver, names_copy,
                                           child / "rc", child / "command.json", child / "launch.json")})
        result_path = _write_capture(out, evidence)
        return {"status": evidence["status"], "output": str(out), "capture": str(result_path)}
    except BaseException as exc:
        evidence.update(status="CAPTURE_REJECTED", rejected=True, failure=f"{type(exc).__name__}: {exc}")
        _write_capture(out, evidence, rejected=True)
        # The outer caller owns this fresh directory and may remove it only after
        # the launcher has proved its child cleanup. Retain rejected diagnostics.
        raise


def main(argv: list[str] | None = None) -> int:
    args = sys.argv[1:] if argv is None else argv
    if len(args) != 1:
        print("usage: nelisp-native-application-harvest.py REQUEST.json", file=sys.stderr)
        return 2
    try:
        print(json.dumps(run_request(args[0]), sort_keys=True))
        return 0
    except Exception as exc:
        print(f"CAPTURE_REJECTED: {type(exc).__name__}: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
