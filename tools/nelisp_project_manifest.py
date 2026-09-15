"""Surgical edits for ordinary dependency tables and guarded file publication."""
import copy
import json
import os
from pathlib import Path
import re
import shutil
import tempfile
import tomllib

from nelisp_packages import dependencies


def edit_dependencies(content, name, requirement):
    """Edit a regular [dependencies] table, checking the complete TOML result.

Inline/dotted dependency representations are readable by the project frontend
but not rewritten here. Refuse any edit whose parsed effect is not exactly
the requested dependency change; comments and other values must survive.
"""
    dependencies({name: requirement if requirement is not None else "0"})
    text = content.decode("utf-8")
    before = tomllib.loads(text)
    expected = copy.deepcopy(before)
    values = expected.setdefault("dependencies", {})
    if requirement is None:
        if name not in values:
            raise ValueError(f"not a direct dependency: {name}")
        del values[name]
    else:
        values[name] = requirement
    newline = "\r\n" if "\r\n" in text else "\n"
    lines = text.splitlines(keepends=True)
    header = re.compile(r'''^[ \t]*\[(?:dependencies|"dependencies"|'dependencies')\][ \t]*(?:#.*)?$''')
    starts = [i for i, line in enumerate(lines) if header.fullmatch(line.rstrip("\r\n"))]
    if not starts:
        if "dependencies" in before:
            raise ValueError("dependency editing requires a regular [dependencies] table")
        result = text + ("" if text.endswith("\n") else newline) + newline
        result += f'[dependencies]{newline}{name} = {json.dumps(requirement)}{newline}'
    else:
        if len(starts) != 1:
            raise ValueError("cannot identify one dependency table safely")
        start = starts[0] + 1
        end = next((i for i in range(start, len(lines)) if lines[i].lstrip().startswith("[")), len(lines))
        assignment = re.compile(r'''^(\s*)([a-z][a-z0-9-]*|"[a-z][a-z0-9-]*"|'[a-z][a-z0-9-]*')\s*=\s*("(?:[^"\\]|\\.)*"|'[^']*')([ \t]*(?:#.*)?)$''')
        matches = []
        for i in range(start, end):
            match = assignment.fullmatch(lines[i].rstrip("\r\n"))
            if match and match[2].strip("\"'") == name:
                matches.append((i, match))
        if len(matches) > 1 or (name in before.get("dependencies", {}) and not matches):
            raise ValueError("dependency editing requires single-line entries in a regular table")
        if matches:
            i, match = matches[0]
            line = lines[i]
            if requirement is not None:
                lines[i] = line[:match.start(3)] + json.dumps(requirement) + line[match.end(3):]
            else:
                comment = match[4].lstrip()
                lines[i] = match[1] + comment + newline if comment else ""
        else:
            if end and not lines[end - 1].endswith("\n"):
                lines[end - 1] += newline
            lines.insert(end, f'{name} = {json.dumps(requirement)}{newline}')
        result = "".join(lines)
    if tomllib.loads(result) != expected:
        raise ValueError("manifest edit did not preserve unrelated TOML values; no files changed")
    return result.encode("utf-8")


def publish_pair(root, manifest_before, lock_before, manifest_after, lock_after):
    """Stage both outputs before publication and roll back a failed second swap.

This is not a crash-atomic two-file transaction. Readers reject a mismatched
manifest/lock pair; writers must not run concurrently. A failed rollback keeps
the original manifest in a named recovery directory instead of deleting it.
"""
    root = Path(root)
    manifest, lock = root / "nelisp.toml", root / "nelisp.lock"
    if manifest.is_symlink() or lock.is_symlink():
        raise ValueError("package editing requires nonsymlink manifest and lock files")
    stage = Path(tempfile.mkdtemp(prefix=".package-edit-", dir=root))
    keep = False
    try:
        original = stage / "original.toml"
        candidate = stage / "nelisp.toml"
        candidate_lock = stage / "nelisp.lock"
        for path, content in [(original, manifest_before), (candidate, manifest_after), (candidate_lock, lock_after)]:
            path.write_bytes(content)
        mode = manifest.stat().st_mode & 0o777
        original.chmod(mode)
        candidate.chmod(mode)
        candidate_lock.chmod(lock.stat().st_mode & 0o777 if lock.exists() else 0o600)
        if manifest.read_bytes() != manifest_before or (lock.read_bytes() if lock.exists() else None) != lock_before:
            raise ValueError("project inputs changed during package editing; retry")
        os.replace(candidate, manifest)
        try:
            os.replace(candidate_lock, lock)
        except BaseException:
            try:
                os.replace(original, manifest)
            except BaseException as rollback:
                keep = True
                raise OSError(f"manifest rollback failed; recover original from {original}") from rollback
            raise
    finally:
        if not keep:
            shutil.rmtree(stage)
