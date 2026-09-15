"""Verified source artifacts for the temporary project frontend."""
import os
from pathlib import Path
import tempfile
from urllib.request import HTTPRedirectHandler, Request, build_opener

from nelisp_packages import release, verify_artifact

MAX_ARTIFACT_BYTES = 16 * 1024 * 1024


class NoRedirect(HTTPRedirectHandler):
    def redirect_request(self, request, fp, code, msg, headers, newurl):
        raise ValueError("package artifact redirects are not supported")


def cache_directory():
    override = os.environ.get("NELISP_CACHE")
    if override:
        return Path(override).expanduser().resolve()
    base = Path(os.environ.get("XDG_CACHE_HOME", Path.home() / ".cache"))
    return base / "nelisp" / "sources-v1"


def source_bytes(locked, *, offline=True):
    """Read verified cached bytes, or explicitly fetch a missing HTTPS artifact.

Every use hashes actual bytes. Corrupt entries fail rather than triggering a
silent replacement. Publication is atomic per artifact, with no executable
file modes, archive extraction, build scripts, or host evaluation.
"""
    locked = release(locked, locked=True)
    directory = cache_directory()
    path = directory / (locked["sha256"] + ".nl")
    if path.exists():
        with path.open("rb") as stream:
            content = stream.read(MAX_ARTIFACT_BYTES + 1)
    else:
        if offline:
            raise ValueError(f"missing cached package {locked['name']}; run nelisp fetch")
        request = Request(locked["url"], headers={"Accept": "application/octet-stream"})
        with build_opener(NoRedirect()).open(request, timeout=30) as response:
            content = response.read(MAX_ARTIFACT_BYTES + 1)
    if len(content) > MAX_ARTIFACT_BYTES:
        raise ValueError(f"package exceeds {MAX_ARTIFACT_BYTES} bytes: {locked['name']}")
    verify_artifact(locked, content)
    content.decode("utf-8")
    if b"\0" in content:
        raise ValueError(f"package source contains NUL: {locked['name']}")
    if not path.exists():
        directory.mkdir(parents=True, exist_ok=True)
        with tempfile.NamedTemporaryFile(dir=directory, prefix=".fetch-", delete=False) as stream:
            temporary = Path(stream.name)
            try:
                stream.write(content)
                stream.close()
                os.replace(temporary, path)
            finally:
                temporary.unlink(missing_ok=True)
    return content
