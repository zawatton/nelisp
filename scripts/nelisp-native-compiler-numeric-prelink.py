"""Certify reviewed numeric source intent and direct prelink ownership only.

Neither signatures nor semantic eligibility are inferred from machine calls.
The independently reviewed source-binding digest is a mandatory input. Actual
runtime memory, roots, exits and numeric semantics still require native proof.
"""
import argparse
import hashlib
import importlib.util
import json
import re
from pathlib import Path

_spec = importlib.util.spec_from_file_location(
    "numeric_prelink_base", Path(__file__).with_name("nelisp-native-rooted-prelink-closure.py"))
_prelink = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_prelink)

CONSTRUCTOR_ROOTS = (
    "nl_root_pin_begin_v2", "nl_root_pin_reserve_v2", "nl_root_pin_end_v2",
    "nl_root_pin_slot_v2", "nl_gc_mark_pinned_roots", "nl_gc_mark_thread_roots",
    "nl_gc_mark_recorded_env", "nl_cold_grow_chunk0", "nl_gc_conserv_owner_slow", "nl_native_cons_v2", "nl_alloc_symbol")
NUMERIC_PARAMS = {
    "wf_any_float_arith": ["list_ptr"],
    "wf_first_non_number_or_bignum": ["args"],
    "wf_fsum": ["list_ptr", "acc_bits", "scratch"],
    "wf_copy32": ["dst", "src"], "wf_sum": ["list_ptr", "acc", "out"],
    "bf_wrong_type_number_or_marker": ["offender"]}
ROOTS = CONSTRUCTOR_ROOTS + tuple(NUMERIC_PARAMS)
DOMAIN = "nelisp-compiler-numeric-prelink-v1"


def _read(path, limit):
    return _prelink._owner.bounded_read(path, limit)


def _digest(raw):
    return hashlib.sha256(raw).hexdigest()


def _sha(value):
    return (isinstance(value, str) and len(value) == 64
            and all(c in "0123456789abcdef" for c in value))


def _source_path(root, relative):
    if not isinstance(relative, str):
        raise ValueError("Source owner path is not a string")
    path = Path(relative)
    result = (root / path).resolve()
    if path.is_absolute() or ".." in path.parts or root not in result.parents:
        raise ValueError("Source owner escapes root")
    return result


def _binding(path, reviewed_digest, root, manifest):
    raw = _read(path, 65536)
    if not _sha(reviewed_digest) or _digest(raw) != reviewed_digest:
        raise ValueError("Source binding differs from independent review pin")
    binding = json.loads(raw)
    if (binding.get("domain") != "nelisp-compiler-numeric-source-binding-v1"
            or binding.get("builder_sha256") != manifest["builder-sha256"]
            or not _sha(binding.get("plus_body_sha256"))):
        raise ValueError("Unknown or stale numeric source binding")
    sources = binding.get("sources")
    if not isinstance(sources, list) or not 1 <= len(sources) <= 16:
        raise ValueError("Numeric source owner bound")
    owners = {}
    for source in sources:
        name, digest = source["path"], source["sha256"]
        if name in owners or not _sha(digest):
            raise ValueError("Duplicate or invalid numeric source owner")
        if _digest(_read(_source_path(root, name), 4 * 1024 * 1024)) != digest:
            raise ValueError("Numeric semantic source changed")
        owners[name] = digest
    if owners.get(manifest["builder-source"]) != manifest["builder-sha256"]:
        raise ValueError("Numeric builder source is not pinned")
    helpers = binding.get("helpers")
    if not isinstance(helpers, list) or [h.get("name") for h in helpers] != list(NUMERIC_PARAMS):
        raise ValueError("Numeric helper policy or order differs")
    for helper in helpers:
        params = helper.get("params")
        if (params != NUMERIC_PARAMS[helper["name"]]
                or helper.get("source") not in owners
                or not _sha(helper.get("body_sha256"))):
            raise ValueError("Numeric helper signature or source binding differs")
    return binding


def _snapshot(manifest, metadata, directory, data_owner, binding, root, reviewed):
    digest = _prelink.verify_manifest(manifest, metadata, directory, data_owner, root)
    document = json.loads(_read(manifest, 65536))
    semantic = _binding(binding, reviewed, root, document)
    return digest, semantic


def _data_extent(entry, data, unit_records, directory):
    """Validate a declared span; zero size remains explicitly unknown."""
    offset, size = entry["offset"], entry["declared_size"]
    if type(offset) is not int or type(size) is not int or offset < 0 or size < 0:
        raise ValueError("Numeric data offset or declared size is invalid")
    if entry["unit"] == data["unit"] and entry["section"] == "bss":
        extent = data.get("bss-size")
    else:
        unit = unit_records.get(entry["unit"])
        if not isinstance(unit, dict):
            raise ValueError("Numeric data unit has no active owner")
        raw = _read(directory / unit["path"], 16 * 1024 * 1024)
        if _digest(raw) != unit["unit-sha256"]:
            raise ValueError("Numeric data unit source changed")
        section = entry["section"].encode("ascii")
        if section == b"bss":
            matches = re.findall(rb"\(bss\s+\.\s+([0-9]+)\)", raw)
            extent = int(matches[0]) if len(matches) == 1 else None
        else:
            matches = re.findall(rb"\(" + section
                                 + rb' :nelisp-cache-bytes-hex "([0-9a-f]*)"\)', raw)
            extent = len(matches[0]) // 2 if len(matches) == 1 and len(matches[0]) % 2 == 0 else None
    if type(extent) is not int or extent <= 0:
        raise ValueError("Numeric data section extent is unverified")
    if offset >= extent or offset + max(size, 1) > extent:
        raise ValueError("Numeric data declared span crosses owner section")
    entry.update(owner_section_size=extent, extent_verified=True,
                 declared_size_known=size != 0)
    if entry["section"] == "bss" and entry["unit"] == data["unit"]:
        entry["owner_bss_size"] = extent
    return entry


def _data_input_shapes(metadata, data_owner):
    """Reject malformed data maps before the shared decoder consumes them."""
    units = json.loads(_read(metadata, 4 * 1024 * 1024))
    data = json.loads(_read(data_owner, 1024 * 1024))
    if (not isinstance(units, list) or not isinstance(data, dict)
            or not isinstance(data.get("unit"), str)
            or type(data.get("bss-size")) is not int):
        raise ValueError("Malformed numeric data owner map")
    for unit in units + [data]:
        if not isinstance(unit, dict) or not isinstance(unit.get("symbols"), list):
            raise ValueError("Malformed numeric data unit map")
        for symbol in unit["symbols"]:
            if (not isinstance(symbol, dict) or not isinstance(symbol.get("name"), str)
                    or not isinstance(symbol.get("section"), str)
                    or type(symbol.get("value")) is not int
                    or type(symbol.get("size", 0)) is not int):
                raise ValueError("Malformed numeric data symbol map")


def certify(manifest, metadata, directory, data_owner, source_root, binding,
            reviewed_binding_sha256, roots=ROOTS, claimed=None):
    """Require immutable genuine inputs and exact direct-closure ownership.

Source-binding contents are independently review-pinned declarations of intent;
they do not establish that the compiler emitted the declared semantics.
"""
    if tuple(roots) != ROOTS:
        raise ValueError("Numeric root policy or order differs")
    _data_input_shapes(metadata, data_owner)
    root = source_root.resolve()
    before, semantic = _snapshot(manifest, metadata, directory, data_owner,
                                 binding, root, reviewed_binding_sha256)
    proof = _prelink.prove(metadata, directory, ROOTS, data_owner, max_functions=192)
    # Record actual data relocation dependencies without promoting them to code.
    units = json.loads(_read(metadata, 4 * 1024 * 1024))
    data = json.loads(_read(data_owner, 1024 * 1024))
    active = json.loads(_read(manifest, 65536))
    unit_records = {unit["name"]: unit for unit in active["units"]}
    data_symbols = {}
    for unit in units + [{"name": data["unit"], "symbols": data["symbols"]}]:
        if not isinstance(unit, dict) or not isinstance(unit.get("symbols"), list):
            raise ValueError("Malformed numeric data unit map")
        names = set()
        for symbol in unit["symbols"]:
            if (not isinstance(symbol, dict) or not isinstance(symbol.get("name"), str)
                    or not isinstance(symbol.get("section"), str)):
                raise ValueError("Malformed numeric data symbol map")
            if symbol["section"] in ("bss", "data", "rodata"):
                if symbol["name"] in names:
                    raise ValueError("Duplicate numeric data symbol owner")
                names.add(symbol["name"])
                entry = dict(name=symbol["name"], unit=unit["name"],
                             section=symbol["section"], offset=symbol.get("value"),
                             declared_size=symbol.get("size", 0))
                old = data_symbols.get(entry["name"])
                if old is not None and old != entry:
                    raise ValueError("Ambiguous numeric data owner")
                data_symbols[entry["name"]] = entry
    owners = _prelink._owner.unit_owners(metadata, directory)
    dependencies = {}
    signatures = []
    by_name = {record["name"]: record for record in proof["records"]}
    for helper in semantic["helpers"]:
        record = by_name[helper["name"]]
        signatures.append(dict(helper, unit=record["unit"],
                               unit_offset=record["unit_offset"], size=record["size"],
                               normalized_sha256=record["normalized_sha256"]))
    for record in proof["records"]:
        for reloc in owners[record["name"]]["relocations"]:
            target = reloc["symbol"]
            if target in data_symbols:
                dependencies[target] = _data_extent(dict(data_symbols[target]), data,
                                                    unit_records, directory)
    after, final_semantic = _snapshot(manifest, metadata, directory, data_owner,
                                      binding, root, reviewed_binding_sha256)
    if before != after or semantic != final_semantic:
        raise ValueError("Numeric provenance changed during verification")
    result = dict(proof, domain=DOMAIN, active_manifest_sha256=before,
                  source_binding_sha256=reviewed_binding_sha256,
                  semantic_intent=dict(plus_body_sha256=semantic["plus_body_sha256"],
                                       helpers=signatures, sources=semantic["sources"]),
                  data_dependencies=[dependencies[k] for k in sorted(dependencies)],
                  helper_count_policy=192,
                  semantics="review-pinned intent; native numeric/root/exit proof pending",
                  generic_call="unsupported")
    if claimed is not None and claimed != result:
        raise ValueError("Claimed numeric certificate drops or changes ownership")
    return result


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("manifest", "metadata", "unit-directory", "generated-data",
                 "source-root", "source-binding", "output"):
        parser.add_argument("--" + name, type=Path, required=True)
    parser.add_argument("--source-binding-sha256", required=True)
    parser.add_argument("--root", action="append")
    args = parser.parse_args()
    certificate = certify(args.manifest, args.metadata, args.unit_directory,
                          args.generated_data, args.source_root, args.source_binding,
                          args.source_binding_sha256, ROOTS if args.root is None else args.root)
    raw = (json.dumps(certificate, indent=2) + "\n").encode()
    args.output.write_bytes(raw)
    print(_digest(raw))
