"""Offline dependency resolution and lock contracts for the project toolchain.

The registry transport and project frontend consume this data-only layer.
One version per package is selected; runtime namespaces are not versioned.
"""
import hashlib
import json
import re
import tomllib
from urllib.parse import urlsplit

NAME = re.compile(r"[a-z][a-z0-9-]*\Z")
NUMBER = r"(?:0|[1-9][0-9]*)"
VERSION = re.compile(rf"{NUMBER}\.{NUMBER}\.{NUMBER}\Z")
REQUIREMENT = re.compile(rf"(\^|=)?({NUMBER}(?:\.{NUMBER}){{0,2}})\Z")
FIELDS = {"version", "dependencies", "sha256", "url"}


def version(text):
    if not isinstance(text, str) or not VERSION.fullmatch(text):
        raise ValueError(f"invalid stable version: {text!r}")
    return tuple(map(int, text.split(".")))


def bounds(text):
    """Bare/caret requirements use the first nonzero compatibility boundary.

Partial zero requirements end at the next specified component: 0 means <1,
0.0 means <0.1. Exact requirements must specify all three components.
"""
    match = REQUIREMENT.fullmatch(text) if isinstance(text, str) else None
    if not match:
        raise ValueError(f"unsupported version requirement: {text!r}")
    operator, number = match.groups()
    parts = tuple(map(int, number.split(".")))
    lower = parts + (0,) * (3 - len(parts))
    if operator == "=":
        if len(parts) != 3:
            raise ValueError("exact requirements need MAJOR.MINOR.PATCH")
        return lower, None
    pivot = next((i for i, part in enumerate(parts) if part), len(parts) - 1)
    upper = lower[:pivot] + (lower[pivot] + 1,) + (0,) * (2 - pivot)
    return lower, upper


def satisfies(candidate, requirement):
    lower, upper = bounds(requirement)
    actual = version(candidate)
    return actual == lower if upper is None else lower <= actual < upper


def dependencies(data):
    if not isinstance(data, dict):
        raise ValueError("dependencies must be a table")
    for name, constraint in data.items():
        if not isinstance(name, str) or not NAME.fullmatch(name):
            raise ValueError(f"invalid package name: {name!r}")
        bounds(constraint)
    return dict(sorted(data.items()))


def release(data, *, locked=False):
    expected = FIELDS | ({"name"} if locked else {"yanked"})
    if not isinstance(data, dict) or set(data) != expected:
        raise ValueError(f"release requires exactly {sorted(expected)}")
    version(data["version"])
    deps = dependencies(data["dependencies"])
    if not isinstance(data["sha256"], str) or not re.fullmatch(r"[0-9a-f]{64}", data["sha256"]):
        raise ValueError("release requires a lowercase SHA-256 digest")
    url = data["url"]
    if not isinstance(url, str) or any(ord(c) <= 32 for c in url):
        raise ValueError("artifact URL must be an HTTPS URL without whitespace")
    parsed = urlsplit(url)
    if parsed.scheme != "https" or not parsed.hostname or parsed.username or parsed.password or parsed.fragment:
        raise ValueError("artifact URL must be HTTPS without credentials or fragments")
    # Accessing port also rejects malformed or out-of-range port strings.
    if parsed.port == 0:
        raise ValueError("artifact URL port must be nonzero")
    if locked:
        if not isinstance(data["name"], str) or not NAME.fullmatch(data["name"]):
            raise ValueError("invalid locked package name")
    elif type(data["yanked"]) is not bool:
        raise ValueError("yanked must be boolean")
    return dict(data, dependencies=deps)


def index_releases(index):
    if (not isinstance(index, dict) or set(index) != {"schema_version", "packages"}
            or type(index["schema_version"]) is not int or index["schema_version"] != 1
            or not isinstance(index["packages"], dict)):
        raise ValueError("unsupported registry index schema")
    result = {}
    for name, entries in index["packages"].items():
        dependencies({name: "0"})
        if not isinstance(entries, list):
            raise ValueError(f"releases for {name} must be an array")
        releases = [release(item) for item in entries]
        if len({item["version"] for item in releases}) != len(releases):
            raise ValueError(f"duplicate release for {name}")
        result[name] = sorted(releases, key=lambda item: version(item["version"]), reverse=True)
    return result


def ordered_closure(requirements, selected):
    """Validate every edge, reject cycles, and emit stable dependency order."""
    ordered, visiting, done = [], set(), set()

    def visit(name, constraint):
        if name not in selected or not satisfies(selected[name]["version"], constraint):
            raise ValueError(f"unsatisfied dependency: {name} {constraint}")
        if name in visiting:
            raise ValueError(f"dependency cycle at {name}")
        if name in done:
            return
        visiting.add(name)
        for child, required in sorted(selected[name]["dependencies"].items()):
            visit(child, required)
        visiting.remove(name)
        done.add(name)
        ordered.append(selected[name])

    for name, constraint in sorted(requirements.items()):
        visit(name, constraint)
    if done != set(selected):
        raise ValueError("lock contains unreachable packages")
    return ordered


def validate_lock(lock):
    if (not isinstance(lock, dict) or set(lock) != {"schema_version", "requirements", "packages"}
            or type(lock["schema_version"]) is not int or lock["schema_version"] != 1
            or not isinstance(lock["packages"], list)):
        raise ValueError("unsupported lock schema")
    requirements = dependencies(lock["requirements"])
    entries = [release(item, locked=True) for item in lock["packages"]]
    selected = {item["name"]: item for item in entries}
    if len(selected) != len(entries):
        raise ValueError("duplicate package in lock")
    return {"schema_version": 1, "requirements": requirements,
            "packages": ordered_closure(requirements, selected)}


def resolve(requirements, index, previous=None):
    """Resolve a graph with backtracking, highest stable versions first.

A previous validated lock prefers matching pinned releases, including yanked
ones. Pass no previous lock for update. Changed metadata for an existing locked
version is an integrity error, even if its replacement would satisfy the graph.
"""
    requirements = dependencies(requirements)
    catalog = index_releases(index)
    pins = {} if previous is None else {p["name"]: p for p in validate_lock(previous)["packages"]}
    for name, pin in pins.items():
        for item in catalog.get(name, []):
            if item["version"] == pin["version"] and any(item[key] != pin[key] for key in FIELDS):
                raise ValueError(f"registry metadata changed for locked package {name}")
    failure = "dependency resolution failed"

    def search(selected, constraints):
        nonlocal failure
        for name, required in sorted(constraints.items()):
            if name in selected and not all(satisfies(selected[name]["version"], r) for r in required):
                failure = f"dependency conflict: {name} requires {', '.join(required)}"
                return None
        remaining = sorted(set(constraints) - set(selected))
        if not remaining:
            try:
                return ordered_closure(requirements, selected)
            except ValueError as error:
                failure = str(error)
                return None
        name = remaining[0]
        candidates = [item for item in catalog.get(name, [])
                      if all(satisfies(item["version"], r) for r in constraints[name])
                      and (not item["yanked"] or pins.get(name, {}).get("version") == item["version"])]
        candidates.sort(key=lambda item: item["version"] != pins.get(name, {}).get("version"))
        if not candidates:
            failure = f"no available release: {name} requires {', '.join(constraints[name])}"
        for item in candidates:
            expanded = {key: list(values) for key, values in constraints.items()}
            for child, required in item["dependencies"].items():
                expanded.setdefault(child, []).append(required)
            locked = {key: item[key] for key in FIELDS}
            result = search(dict(selected, **{name: dict(locked, name=name)}), expanded)
            if result is not None:
                return result
        return None

    entries = search({}, {name: [required] for name, required in requirements.items()})
    if entries is None:
        raise ValueError(failure)
    return {"schema_version": 1, "requirements": requirements, "packages": entries}


def lock_bytes(lock):
    """Canonical UTF-8 TOML; no host paths, timestamps, or registry ordering."""
    lock = validate_lock(lock)
    def quote(value):
        return json.dumps(value, ensure_ascii=False)
    lines = ["schema_version = 1", ""]
    if not lock["packages"]:
        lines.extend(["packages = []", ""])
    lines.append("[requirements]")
    lines.extend(f"{quote(name)} = {quote(value)}" for name, value in lock["requirements"].items())
    for item in lock["packages"]:
        lines.extend(["", "[[packages]]"])
        lines.extend(f"{key} = {quote(item[key])}" for key in ("name", "version", "sha256", "url"))
        lines.append("[packages.dependencies]")
        lines.extend(f"{quote(name)} = {quote(value)}" for name, value in item["dependencies"].items())
    return ("\n".join(lines) + "\n").encode("utf-8")


def read_lock(content):
    return validate_lock(tomllib.loads(content.decode("utf-8")))


def verify_artifact(locked, content):
    """Verify downloaded/cached bytes before a consumer parses or executes them."""
    locked = release(locked, locked=True)
    if hashlib.sha256(content).hexdigest() != locked["sha256"]:
        raise ValueError(f"artifact integrity mismatch: {locked['name']} {locked['version']}")
    return content
