"""Prove bounded direct ownership from selected compiler units before linking."""
import argparse
import hashlib
import importlib.util
import json
import re
from pathlib import Path

from capstone import Cs, CS_ARCH_X86, CS_MODE_64, CS_GRP_CALL, CS_GRP_JUMP
from capstone.x86 import X86_OP_IMM

_spec = importlib.util.spec_from_file_location(
    "rooted_unit_owner", Path(__file__).with_name("nelisp-native-rooted-direct-closure.py"))
_owner = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_owner)


def verify_manifest(manifest, metadata, directory, data_owner, source_root):
    """Bind generation inputs to the actual active build, never a stale selection."""
    raw = _owner.bounded_read(manifest, 65536)
    document = json.loads(raw)
    if document.get("domain") != "nelisp-rooted-active-build-v1":
        raise ValueError("Unknown active build manifest")
    relative = Path(document["builder-source"])
    if relative.is_absolute() or ".." in relative.parts:
        raise ValueError("Builder source provenance escapes root")
    root = source_root.resolve()
    builder = (root / relative).resolve()
    if root not in builder.parents:
        raise ValueError("Builder source owner escapes root")
    if hashlib.sha256(_owner.bounded_read(builder, 4 * 1024 * 1024)).hexdigest() != document["builder-sha256"]:
        raise ValueError("Missing or changed builder source")
    metadata_bytes = _owner.bounded_read(metadata, 4 * 1024 * 1024)
    data_bytes = _owner.bounded_read(data_owner, 1024 * 1024)
    if (hashlib.sha256(metadata_bytes).hexdigest() != document["metadata-sha256"]
            or hashlib.sha256(data_bytes).hexdigest() != document["generated-data-sha256"]):
        raise ValueError("Changed active metadata or data owner")
    records = document["units"]
    if not isinstance(records, list) or not 1 <= len(records) <= 128:
        raise ValueError("Active build unit count bound")
    selected, text_units, total = {}, set(), 0
    for record in records:
        name, filename = record["name"], record["path"]
        if (name in selected or Path(filename).name != filename
                or Path(name).name != name):
            raise ValueError("Duplicate or malformed active unit selection")
        path = (directory / filename).resolve()
        if path.parent != directory.resolve():
            raise ValueError("Active unit file escapes owner directory")
        unit_bytes = _owner.bounded_read(path, 16 * 1024 * 1024)
        total += len(unit_bytes)
        if total > 32 * 1024 * 1024:
            raise ValueError("Active unit aggregate input bound")
        if hashlib.sha256(unit_bytes).hexdigest() != record["unit-sha256"]:
            raise ValueError("Missing or changed active unit source")
        if re.search(rb'\(text :nelisp-cache-bytes-hex "[0-9a-f]+"', unit_bytes):
            text_units.add(name)
        selected[name] = record
    units = json.loads(metadata_bytes)
    if len(units) != len(text_units) or {unit["name"] for unit in units} != text_units:
        raise ValueError("Incomplete active text unit metadata")
    for unit in units:
        record = selected[unit["name"]]
        if unit["path"] != record["path"] or unit["unit-sha256"] != record["unit-sha256"]:
            raise ValueError("Active unit metadata differs from manifest")
    data = json.loads(data_bytes)
    if data["unit"] not in selected or data["owner-source-sha256"] != document["builder-sha256"]:
        raise ValueError("Generated data owner is not active build source")
    return hashlib.sha256(raw).hexdigest()


def prove(metadata, directory, roots, data_owner, claimed=None, max_functions=128, evaluator_boundary=None):
    """Reject unresolved edges; this source certificate grants no runtime capability."""
    if evaluator_boundary not in (None, "nl_apply_function"):
        raise ValueError("Unknown evaluator boundary")
    if max_functions not in (128, 192, 200, 272, 280):
        raise ValueError("Unknown direct helper count policy")
    if max_functions in (200, 272, 280) and "nl_native_frame_v2" not in roots:
        raise ValueError("Frame closure bound requires the authenticated frame root")
    owners = _owner.unit_owners(metadata, directory)
    units = json.loads(_owner.bounded_read(metadata, 4 * 1024 * 1024))
    data_bytes = _owner.bounded_read(data_owner, 1024 * 1024)
    data = json.loads(data_bytes)
    if not 0 < data.get("bss-size", 0) <= 16 * 1024 * 1024:
        raise ValueError("Generated data owner size bound")
    data_names = set()
    for symbol in data.get("symbols", []):
        if symbol["section"] == "bss":
            if not 0 <= symbol["value"] < data["bss-size"] or symbol["name"] in data_names:
                raise ValueError("Generated data boundary or duplicate owner")
            data_names.add(symbol["name"])
    # Only these reader-owned kernel32 terminals extend the Win64 closure.
    # Runtime issuance additionally proves PE thunk bytes, IAT name and live address.
    os_imports = {"VirtualAlloc", "VirtualFree", "ExitProcess"} if data.get("target") == "windows-x86_64" else set()
    known = set(owners) | data_names | os_imports
    for unit in units:
        for symbol in unit["symbols"]:
            if symbol["section"] in ("rodata", "data", "bss"):
                known.add(symbol["name"])
    decoder = Cs(CS_ARCH_X86, CS_MODE_64)
    decoder.detail = True
    pending = list(roots)
    records, seen, total = [], set(), 0
    while pending:
        name = pending.pop(0)
        if name in seen:
            continue
        if name not in owners:
            raise ValueError("Missing direct function owner: " + name)
        if len(seen) >= max_functions:
            raise ValueError("Direct helper count bound")
        owner = owners[name]
        start, end = owner["start"], owner["end"]
        size = end - start
        if not 0 < size <= 65536 or total + size > 512 * 1024:
            raise ValueError("Direct helper text size bound")
        normalized = bytearray(owner["text"][start:end])
        relocations = {}
        for relocation in owner["relocations"]:
            position = relocation["offset"] - start
            if (relocation["type"] not in ("pc32", "plt32")
                    or not 0 <= position <= size - 4
                    or relocation["symbol"] not in known
                    or position in relocations):
                raise ValueError(f"Unknown, overlapping or crossing prelink relocation: owner={name} "
                                 f"offset={position} size={size} type={relocation['type']} "
                                 f"symbol={relocation['symbol']} known={relocation['symbol'] in known}")
            relocations[position] = relocation
            normalized[position:position + 4] = bytes(4)
        direct, decoded = [], 0
        for instruction in decoder.disasm(owner["text"][start:end], start):
            decoded += instruction.size
            if not (instruction.group(CS_GRP_CALL) or instruction.group(CS_GRP_JUMP)):
                continue
            if len(instruction.operands) != 1 or instruction.operands[0].type != X86_OP_IMM:
                raise ValueError("Indirect control flow refused: " + name)
            position = instruction.address - start
            relocation = relocations.get(position + instruction.imm_offset)
            target = None
            if relocation:
                if instruction.imm_size != 4 or relocation["addend"] != 0:
                    raise ValueError("Unknown direct relocation encoding")
                target = relocation["symbol"]
            else:
                address = instruction.operands[0].imm
                target = owner["starts"].get(address)
                if target is None and start <= address < end:
                    continue
            if target not in owners and target not in os_imports:
                raise ValueError("Unresolved direct control flow: " + name)
            direct.append(dict(offset=position, mnemonic=instruction.mnemonic, target=target))
            if name != evaluator_boundary and target not in os_imports:
                pending.append(target)
        if decoded != size:
            raise ValueError("Incomplete prelink instruction decoding")
        records.append(dict(name=name, unit=owner["unit"], unit_offset=start, size=size,
                            normalized_sha256=hashlib.sha256(normalized).hexdigest(),
                            direct=direct, indirect=[],
                            **({"evaluator_boundary": True} if name == evaluator_boundary else {})))
        seen.add(name)
        total += size
    result = dict(domain="nelisp-rooted-prelink-text-v1", roots=list(roots), records=records,
                  total_bytes=total, status="PRELINK_DIRECT_CLOSURE_PASS",
                  metadata_sha256=hashlib.sha256(
                      _owner.bounded_read(metadata, 4 * 1024 * 1024)).hexdigest(),
                  generated_data_sha256=hashlib.sha256(data_bytes).hexdigest(),
                  runtime_memory="pending", operation_eligibility=[])
    if os_imports:
        result["os_imports"] = sorted(os_imports)
    if claimed is not None and claimed != result:
        raise ValueError("Claimed prelink proof drops or changes ownership")
    return result


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--metadata", type=Path, required=True)
    parser.add_argument("--unit-directory", type=Path, required=True)
    parser.add_argument("--generated-data", type=Path, required=True)
    parser.add_argument("--root", action="append", required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--manifest", type=Path)
    parser.add_argument("--source-root", type=Path)
    args = parser.parse_args()
    if bool(args.manifest) != bool(args.source_root):
        parser.error("--manifest and --source-root are required together")
    digest = (verify_manifest(args.manifest, args.metadata, args.unit_directory,
                              args.generated_data, args.source_root)
              if args.manifest else None)
    result = prove(args.metadata, args.unit_directory, args.root, args.generated_data)
    if digest is not None:
        if digest != verify_manifest(args.manifest, args.metadata, args.unit_directory,
                                     args.generated_data, args.source_root):
            raise ValueError("Build provenance changed during prelink proof")
        result["active_manifest_sha256"] = digest
    output = (json.dumps(result, indent=2) + "\n").encode("utf-8")
    args.output.write_bytes(output)
    print(hashlib.sha256(output).hexdigest())
