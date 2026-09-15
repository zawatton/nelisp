"""Explicit HTTPS registry snapshots with validated offline reuse."""
import hashlib
import json
import os
from pathlib import Path
import tempfile
from urllib.parse import urlsplit
from urllib.request import Request, build_opener

from nelisp_package_store import NoRedirect, cache_directory
from nelisp_packages import index_releases

MAX_INDEX_BYTES = 16 * 1024 * 1024


def parse_index(content):
    if len(content) > MAX_INDEX_BYTES:
        raise ValueError("registry index exceeds size limit")

    def unique_pairs(pairs):
        result = {}
        for name, value in pairs:
            if name in result:
                raise ValueError(f"duplicate registry field: {name}")
            result[name] = value
        return result

    index = json.loads(content.decode("utf-8"), object_pairs_hook=unique_pairs)
    index_releases(index)
    return index


def load_index(local=None, registry=None, *, offline=False):
    """Read a local trust input or fetch an explicitly selected HTTPS index.

TLS authenticates the selected server. Cached digests detect accidental
corruption, not malicious local replacement or registry compromise. Signed
indexes and rollback/freshness policy remain separate unfinished contracts.
"""
    if local is not None:
        with Path(local).open("rb") as stream:
            return parse_index(stream.read(MAX_INDEX_BYTES + 1))
    url = registry or os.environ.get("NELISP_REGISTRY")
    if not url:
        raise ValueError("select --index FILE, --registry HTTPS_URL, or NELISP_REGISTRY")
    parsed = urlsplit(url)
    if (parsed.scheme != "https" or not parsed.hostname or parsed.username or parsed.password
            or parsed.fragment or any(ord(c) <= 32 for c in url) or parsed.port == 0):
        raise ValueError("registry must be an HTTPS index URL without credentials or fragments")
    key = hashlib.sha256(url.encode("utf-8")).hexdigest()
    directory = cache_directory() / "indexes-v1"
    path = directory / (key + ".json")
    if offline:
        if not path.exists():
            raise ValueError("no cached registry index; fetch it online first")
        with path.open("rb") as stream:
            content = stream.read(MAX_INDEX_BYTES * 6 + 4097)
        if len(content) > MAX_INDEX_BYTES * 6 + 4096:
            raise ValueError("cached registry snapshot exceeds size limit")
        envelope = json.loads(content.decode("utf-8"))
        if (not isinstance(envelope, dict) or set(envelope) != {"url", "sha256", "index"}
                or envelope["url"] != url or not isinstance(envelope["index"], str)):
            raise ValueError("invalid cached registry snapshot")
        raw = envelope["index"].encode("utf-8")
        if hashlib.sha256(raw).hexdigest() != envelope["sha256"]:
            raise ValueError("cached registry integrity mismatch")
        return parse_index(raw)
    request = Request(url, headers={"Accept": "application/json"})
    with build_opener(NoRedirect()).open(request, timeout=30) as response:
        raw = response.read(MAX_INDEX_BYTES + 1)
    index = parse_index(raw)
    envelope = {"url": url, "sha256": hashlib.sha256(raw).hexdigest(), "index": raw.decode("utf-8")}
    directory.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(dir=directory, prefix=".index-", delete=False) as stream:
        temporary = Path(stream.name)
        try:
            stream.write(json.dumps(envelope, ensure_ascii=False).encode("utf-8"))
            stream.close()
            os.replace(temporary, path)
        finally:
            temporary.unlink(missing_ok=True)
    return index


def search_index(index, query):
    """Return deterministic name/version results, excluding yanked releases."""
    releases = index_releases(index)
    result = []
    for name in sorted(releases):
        available = [item for item in releases[name] if not item["yanked"]]
        if query.lower() in name and available:
            result.append({"name": name, "version": available[0]["version"]})
    return result
