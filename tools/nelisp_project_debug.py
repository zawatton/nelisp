"""Immutable source maps bound to a debug executable's embedded digest."""
import hashlib
import json
from pathlib import Path

MARKER = b"\0NELISP_DEBUG_MAP_V1:"


def canonical(value):
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"), allow_nan=False).encode("utf-8")


def make_map(inputs, plan, entry):
    """Build a nonexecuting declaration index over the exact build snapshots."""
    files = []
    symbols = []
    for (path, content), item in zip(inputs, plan, strict=True):
        files.append({"path": path, "sha256": hashlib.sha256(content).hexdigest(),
                      "text": content.decode("utf-8")})
        symbols.extend(dict(symbol, path=path) for symbol in item["symbols"])
    return {"schema_version": 1, "scope": "source-declaration-spans", "entry": entry,
            "files": files, "symbols": symbols}


def digest(source_map):
    return hashlib.sha256(canonical(source_map)).hexdigest()


def envelope(source_map, binary):
    return {"map": source_map, "map_sha256": digest(source_map),
            "binary_sha256": hashlib.sha256(binary).hexdigest()}


def inspect(binary_path, name=None):
    """Verify both artifacts before returning any source location or text."""
    binary_path = Path(binary_path)
    binary = binary_path.read_bytes()
    if binary[:7] != b"\x7fELF\x02\x01\x01" or binary[16:20] != b"\x02\x00\x3e\x00":
        raise ValueError("debug-info requires a Linux x86_64 executable")
    record = json.loads(Path(str(binary_path) + ".debug.json").read_text(encoding="utf-8"))
    if not isinstance(record, dict) or set(record) != {"map", "map_sha256", "binary_sha256"}:
        raise ValueError("invalid debug map envelope")
    source_map = record["map"]
    if (not isinstance(source_map, dict) or source_map.get("schema_version") != 1
            or source_map.get("scope") != "source-declaration-spans"
            or not isinstance(source_map.get("entry"), str)
            or not isinstance(source_map.get("files"), list)
            or not isinstance(source_map.get("symbols"), list)):
        raise ValueError("unsupported debug map schema")
    expected = digest(source_map)
    if (record["binary_sha256"] != hashlib.sha256(binary).hexdigest()
            or record["map_sha256"] != expected
            or binary.count(MARKER + expected.encode("ascii") + b"\0") != 1):
        raise ValueError("debug map does not match the executable")
    try:
        files = {item["path"]: item["text"].encode("utf-8") for item in source_map["files"]}
        for item in source_map["files"]:
            if hashlib.sha256(files[item["path"]]).hexdigest() != item["sha256"]:
                raise ValueError("debug source hash mismatch")
        selected = []
        for symbol in source_map["symbols"]:
            if name is not None and symbol["symbol"] != name:
                continue
            data = files[symbol["path"]]
            start, end = symbol["start"]["byte_offset"], symbol["end"]["byte_offset"]
            if type(start) is not int or type(end) is not int or not 0 <= start <= end <= len(data):
                raise ValueError("invalid debug source span")
            selected.append(dict(symbol, text=data[start:end].decode("utf-8")))
    except (KeyError, TypeError, AttributeError) as error:
        raise ValueError("invalid debug map structure") from error
    if name is not None and not selected:
        raise ValueError(f"no recorded declaration: {name}")
    return {"schema_version": 1, "scope": source_map["scope"], "binary_sha256": record["binary_sha256"],
            "map_sha256": expected, "entry": source_map["entry"], "symbols": selected}
