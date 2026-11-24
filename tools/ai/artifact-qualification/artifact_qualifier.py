#!/usr/bin/env python3
"""Fail-closed validation for a format-v1 sealed input bundle."""

from __future__ import annotations

import argparse
import errno
import hashlib
import json
import os
import re
import stat
import sys
from pathlib import Path, PurePosixPath


class BundleError(ValueError):
    def __init__(self, category: str, detail: str):
        self.category = category
        super().__init__(f"{category}: {detail}")


def _fail(category: str, detail: str) -> None:
    raise BundleError(category, detail)


def _unique_object(pairs):
    obj = {}
    for key, value in pairs:
        if key in obj:
            _fail("DUPLICATE_JSON_KEY", f"duplicate JSON object key: {key}")
        obj[key] = value
    return obj


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _namespace_path(value: object, field: str) -> str:
    if not isinstance(value, str) or not value or "\0" in value:
        _fail("SCHEMA", f"{field} must be a nonempty path")
    path = PurePosixPath(value)
    if not value.startswith("/") or value.startswith("//") or not path.is_absolute() or ".." in path.parts or str(path) != value or (value == "/" and field != "cwd"):
        _fail("PATH", f"{field} must be a normalized absolute path")
    if any(part in ("proc", "dev") for part in path.parts[1:2]):
        _fail("RESERVED", f"{field} overlaps /proc or /dev")
    return str(path)


def _bundle_source(root: Path, value: object) -> Path:
    if not isinstance(value, str) or not value or "\0" in value:
        _fail("SCHEMA", "source must be a nonempty relative path")
    rel = PurePosixPath(value)
    if rel.is_absolute() or ".." in rel.parts or str(rel) != value or value in (".", ""):
        _fail("PATH", f"invalid bundle-relative source: {value!r}")
    candidate = root.joinpath(*rel.parts)
    try:
        resolved = candidate.resolve(strict=True)
        resolved.relative_to(root.resolve(strict=True))
    except RuntimeError:
        _fail("LINK_CYCLE", f"symlink cycle in bundle: {value}")
    except OSError as exc:
        if exc.errno == errno.ELOOP:
            _fail("LINK_CYCLE", f"symlink cycle in bundle: {value}")
        _fail("MISSING_OR_ESCAPE", f"source missing or escapes bundle: {value}")
    except ValueError:
        _fail("MISSING_OR_ESCAPE", f"source missing or escapes bundle: {value}")
    return candidate


def validate_bundle(manifest_path, bundle_root, expected_manifest_sha256):
    """Validate a sealed bundle and return its parsed manifest on success.

    The expected hash is checked over raw manifest bytes before JSON parsing.
    This is input validation only: it neither invokes argv nor stages a namespace.
    """
    if not isinstance(expected_manifest_sha256, str) or not re.fullmatch(r"[0-9a-f]{64}", expected_manifest_sha256):
        _fail("EXPECTED_HASH", "an externally supplied lowercase SHA256 is required")
    manifest_file = Path(manifest_path)
    try:
        raw = manifest_file.read_bytes()
    except OSError as exc:
        _fail("MANIFEST_IO", str(exc))
    if hashlib.sha256(raw).hexdigest() != expected_manifest_sha256:
        _fail("MANIFEST_HASH", "manifest does not match externally supplied hash")
    try:
        manifest = json.loads(raw, object_pairs_hook=_unique_object)
    except (UnicodeDecodeError, json.JSONDecodeError) as exc:
        _fail("SCHEMA", f"manifest is not valid UTF-8 JSON: {exc}")
    if not isinstance(manifest, dict) or set(manifest) != {"format", "inputs", "execution"} or manifest["format"] != "artifact-bundle-v1":
        _fail("SCHEMA", "expected exact artifact-bundle-v1 top-level schema")
    rows, execution = manifest["inputs"], manifest["execution"]
    if not isinstance(rows, list) or not rows or not isinstance(execution, dict) or set(execution) != {"argv", "cwd", "env", "private_outputs"}:
        _fail("SCHEMA", "inputs and exact execution declaration are required")

    root = Path(bundle_root).resolve(strict=True)
    if not root.is_dir():
        _fail("BUNDLE", "bundle root must be a directory")
    mapped = {}
    for row in rows:
        if not isinstance(row, dict) or set(row) - {"source", "destination", "type", "mode", "sha256", "target"}:
            _fail("SCHEMA", "input row has unknown fields or is not an object")
        if not {"source", "destination", "type", "mode"} <= set(row):
            _fail("SCHEMA", "input row is missing required fields")
        dest = _namespace_path(row["destination"], "destination")
        if dest in mapped:
            _fail("DUPLICATE_PATH", f"duplicate namespace destination {dest}")
        if any(dest.startswith(parent + "/") or parent.startswith(dest + "/") for parent in mapped):
            _fail("CONFLICTING_PATH", f"file path conflicts with another mapping: {dest}")
        mode = row["mode"]
        if mode not in ("0444", "0555"):
            _fail("MODE", f"input must be read-only mode 0444 or 0555: {dest}")
        kind = row["type"]
        if kind not in ("file", "symlink"):
            _fail("SCHEMA", f"unsupported input type at {dest}")
        source = _bundle_source(root, row["source"])
        st = source.lstat()
        if kind == "file":
            if not stat.S_ISREG(st.st_mode) or stat.S_IMODE(st.st_mode) != int(mode, 8):
                _fail("TYPE_OR_MODE", f"file type/mode mismatch: {row['source']}")
            if not re.fullmatch(r"[0-9a-f]{64}", str(row.get("sha256", ""))) or _sha256(source) != row["sha256"]:
                _fail("FILE_HASH", f"file hash mismatch: {row['source']}")
            if "target" in row:
                _fail("SCHEMA", "regular file cannot declare target")
        else:
            if not stat.S_ISLNK(st.st_mode) or "sha256" in row:
                _fail("TYPE_OR_MODE", f"symlink type/hash mismatch: {row['source']}")
            target = _namespace_path(row.get("target"), "symlink target")
            actual_target = os.readlink(source)
            resolved_target = (source.parent / actual_target).resolve(strict=True)
            try:
                resolved_target.relative_to(root)
            except ValueError:
                _fail("LINK_ESCAPE", f"symlink escapes bundle: {row['source']}")
        mapped[dest] = (kind, row, source)

    private_outputs = execution["private_outputs"]
    if not isinstance(private_outputs, list) or not private_outputs:
        _fail("SCHEMA", "private_outputs must be a nonempty list")
    outputs = [_namespace_path(p, "private output") for p in private_outputs]
    if len(set(outputs)) != len(outputs):
        _fail("DUPLICATE_PATH", "duplicate private output")
    if any(a.startswith(b + "/") or b.startswith(a + "/") for i, a in enumerate(outputs) for b in outputs[i + 1:]):
        _fail("CONFLICTING_PATH", "nested private output paths overlap")
    for output in outputs:
        if any(output == p or output.startswith(p + "/") or p.startswith(output + "/") for p in mapped):
            _fail("OUTPUT_OVERLAP", f"private output overlaps input: {output}")
    argv, cwd, env = execution["argv"], execution["cwd"], execution["env"]
    if not isinstance(argv, list) or not argv or any(not isinstance(x, str) or not x or "\0" in x for x in argv):
        _fail("SCHEMA", "argv must be a nonempty array of nonempty strings")
    if not isinstance(env, dict) or any(not isinstance(k, str) or not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", k) or not isinstance(v, str) or "\0" in v for k, v in env.items()):
        _fail("SCHEMA", "env must be an explicit string-to-string map")
    cwd_path = _namespace_path(cwd, "cwd")
    if cwd_path not in ("/",) and not any(p.startswith(cwd_path.rstrip("/") + "/") for p in mapped):
        _fail("SCHEMA", "cwd has no declared inputs beneath it")
    executable = _namespace_path(argv[0], "argv[0]")
    entry = mapped.get(executable)
    if not entry or entry[0] != "file" or entry[1]["mode"] != "0555":
        _fail("UNPINNED_EXECUTABLE", "argv[0] must name a pinned executable file with mode 0555")

    # Every bundle leaf must be declared; no host fallback is permitted.
    actual_sources = set()
    for base, dirs, files in os.walk(root, followlinks=False):
        base_path = Path(base)
        for name in list(dirs):
            path = base_path / name
            if path.is_symlink():
                dirs.remove(name)
                actual_sources.add(str(path))
        actual_sources.update(str(base_path / name) for name in files)
    declared_sources = {str(_bundle_source(root, row["source"])) for row in rows}
    if actual_sources != declared_sources:
        _fail("BUNDLE_CONTENTS", "bundle has missing or undeclared filesystem entries")
    for kind, row, source in mapped.values():
        if kind == "symlink":
            link_dest = row["target"]
            try:
                target_source = (source.parent / os.readlink(source)).resolve(strict=True)
            except RuntimeError:
                _fail("LINK_CYCLE", f"symlink cycle in bundle: {row['source']}")
            declared_target = mapped.get(link_dest)
            if not declared_target or declared_target[0] != "file" or declared_target[2].resolve(strict=True) != target_source:
                _fail("LINK_ALLOWLIST", f"symlink target is not an explicitly mapped input: {row['source']}")
    return manifest


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("manifest")
    parser.add_argument("bundle_root")
    parser.add_argument("--expected-sha256", required=True)
    args = parser.parse_args(argv)
    try:
        validate_bundle(args.manifest, args.bundle_root, args.expected_sha256)
    except BundleError as exc:
        print(str(exc), file=sys.stderr)
        return 2
    print("VALIDATED")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
