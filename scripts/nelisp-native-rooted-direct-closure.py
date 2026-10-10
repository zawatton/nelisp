"""Bound direct native ownership by decoded unit text, never relocation lists alone."""
import argparse
import hashlib
import json
import re
from pathlib import Path

from capstone import Cs, CS_ARCH_X86, CS_MODE_64, CS_GRP_CALL, CS_GRP_JUMP
from capstone.x86 import X86_OP_IMM
from elftools.elf.elffile import ELFFile

MAX_FUNCTIONS = 128
MAX_FUNCTION_BYTES = 65536
MAX_TOTAL_BYTES = 512 * 1024


def bounded_read(path, limit):
    if not 0 < path.stat().st_size <= limit:
        raise ValueError("File eligibility bound: " + path.name)
    return path.read_bytes()


def unit_owners(metadata, directory):
    """Retain every function boundary, including unexported same-unit helpers."""
    owners = {}
    units = json.loads(bounded_read(metadata, 4 * 1024 * 1024))
    if not isinstance(units, list) or len(units) > 128:
        raise ValueError("Unit metadata bound")
    total_unit_bytes = 0
    for unit in units:
        path = directory / Path(unit["path"]).name
        if path.resolve().parent != directory.resolve():
            raise ValueError("Unit source escapes its owner directory")
        total_unit_bytes += path.stat().st_size
        if total_unit_bytes > 32 * 1024 * 1024:
            raise ValueError("Aggregate unit source input bound")
        raw = bounded_read(path, 16 * 1024 * 1024)
        if hashlib.sha256(raw).hexdigest() != unit["unit-sha256"]:
            raise ValueError("Unit source hash differs")
        match = re.search(rb"\(text :nelisp-cache-bytes-hex \"([0-9a-f]+)\"", raw)
        if not match:
            raise ValueError("Unknown unit text encoding")
        text = bytes.fromhex(match[1].decode("ascii"))
        functions = sorted((symbol for symbol in unit["symbols"]
                            if symbol["section"] == "text" and symbol["type"] == "func"),
                           key=lambda symbol: symbol["value"])
        if len(functions) > 20000 or len({f["value"] for f in functions}) != len(functions):
            raise ValueError("Ambiguous unit function boundaries")
        starts = {f["value"]: f["name"] for f in functions}
        for index, function in enumerate(functions):
            start = function["value"]
            end = functions[index + 1]["value"] if index + 1 < len(functions) else len(text)
            if not 0 <= start < end <= len(text) or function["name"] in owners:
                raise ValueError("Invalid or duplicate unit function owner: " + function["name"]
                                 + " in " + unit["name"])
            relocations = [r for r in unit["relocations"]
                           if r["section"] == "text" and start <= r["offset"] < end]
            owners[function["name"]] = dict(unit=unit["name"], start=start, end=end,
                                            text=text, starts=starts, relocations=relocations,
                                            unit_sha256=unit["unit-sha256"])
    return owners


def prove(image, metadata, directory, roots, claimed=None, max_functions=128, evaluator_boundary=None):
    if not 0 < image.stat().st_size <= 128 * 1024 * 1024:
        raise ValueError("Image eligibility bound")
    owners = unit_owners(metadata, directory)
    decoder = Cs(CS_ARCH_X86, CS_MODE_64)
    decoder.detail = True
    records, total = [], 0
    queue, visited = list(roots), set()
    with image.open("rb") as stream:
        if stream.read(2) == b'MZ':
            import importlib.util
            spec = importlib.util.spec_from_file_location('pe_image', Path(__file__).with_name('nelisp-native-pe-image.py'))
            pe_module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(pe_module)
            stream.seek(0)
            elf = pe_module.PEImage(stream)
            terminals = {'ExitProcess', 'VirtualAlloc', 'VirtualFree'}
        else:
            stream.seek(0)
            elf = ELFFile(stream)
            terminals = set()
        if elf["e_machine"] != "EM_X86_64" or elf["e_type"] != "ET_EXEC":
            raise ValueError("Unsupported native image domain")
        table = elf.get_section_by_name(".symtab")
        if table is None or table.num_symbols() > 20000:
            raise ValueError("ELF symbol bound")
        symbols = {}
        for symbol in table.iter_symbols():
            if symbol.name:
                if symbol.name in symbols and symbols[symbol.name]["st_value"] != symbol["st_value"]:
                    raise ValueError("Ambiguous linked symbol")
                symbols[symbol.name] = symbol
        while queue:
            name = queue.pop(0)
            if name in visited:
                continue
            if name in terminals:
                elf.verify_terminal(name)
                visited.add(name)
                continue
            if name not in owners or name not in symbols:
                raise ValueError("Unknown direct owner: " + name)
            owner, symbol = owners[name], symbols[name]
            size = owner["end"] - owner["start"]
            if not 0 < size <= MAX_FUNCTION_BYTES:
                raise ValueError("Function byte bound: %s (%d)" % (name, size))
            if len(records) >= max_functions or total + size > MAX_TOTAL_BYTES:
                raise ValueError("Direct helper aggregate bound at %s: functions=%d bytes=%d next=%d"
                                 % (name, len(records), total, size))
            section = elf.get_section(symbol["st_shndx"])
            address = symbol["st_value"]
            offset = address - section["sh_addr"]
            if (symbol["st_info"]["type"] != "STT_FUNC" or section["sh_type"] != "SHT_PROGBITS"
                    or not section["sh_flags"] & 4 or offset < 0 or offset + size > section["sh_size"]):
                raise ValueError("Linked function boundary differs")
            stream.seek(section["sh_offset"] + offset)
            linked = stream.read(size)
            if len(linked) != size:
                raise ValueError("Truncated linked function")
            unit_text = owner["text"][owner["start"]:owner["end"]]
            normalized_unit, normalized_linked = bytearray(unit_text), bytearray(linked)
            relocations = {}
            used = set()
            for relocation in owner["relocations"]:
                position = relocation["offset"] - owner["start"]
                if (relocation["type"] not in ("pc32", "plt32") or position < 0
                        or position + 4 > size or used.intersection(range(position, position + 4))):
                    raise ValueError("Unknown or crossing unit relocation")
                target = symbols.get(relocation["symbol"])
                if target is None or target["st_shndx"] == "SHN_UNDEF":
                    raise ValueError("Unresolved linked relocation")
                actual = int.from_bytes(linked[position:position + 4], "little", signed=True)
                expected = target["st_value"] + relocation["addend"] - (address + position + 4)
                if actual != expected:
                    raise ValueError("Relocation target differs")
                used.update(range(position, position + 4))
                relocations[position] = relocation
                normalized_unit[position:position + 4] = bytes(4)
                normalized_linked[position:position + 4] = bytes(4)
            if normalized_unit != normalized_linked:
                raise ValueError("Linked text differs from genuine unit: " + name)
            direct, indirect, decoded = [], [], 0
            for instruction in decoder.disasm(unit_text, owner["start"]):
                decoded += instruction.size
                if not (instruction.group(CS_GRP_CALL) or instruction.group(CS_GRP_JUMP)):
                    continue
                position = instruction.address - owner["start"]
                if len(instruction.operands) != 1 or instruction.operands[0].type != X86_OP_IMM:
                    indirect.append(dict(offset=position, mnemonic=instruction.mnemonic))
                    continue
                relocation = relocations.get(position + instruction.imm_offset)
                target_offset = instruction.operands[0].imm
                if relocation:
                    target_name = relocation["symbol"]
                    if symbols[target_name]["st_info"]["type"] != "STT_FUNC":
                        raise ValueError("Direct control transfer targets data")
                elif (instruction.group(CS_GRP_JUMP)
                      and owner["start"] <= target_offset < owner["end"]):
                    continue
                else:
                    target_name = owner["starts"].get(target_offset)
                    if target_name is None:
                        raise ValueError("Unknown same-unit direct target in " + name)
                    target = symbols.get(target_name)
                    if target is None or target["st_value"] != address + target_offset - owner["start"]:
                        raise ValueError("Same-unit linked target differs")
                direct.append(dict(offset=position, mnemonic=instruction.mnemonic, target=target_name))
                if name != evaluator_boundary:
                    queue.append(target_name)
            if decoded != size:
                raise ValueError("Incomplete instruction decoding: " + name)
            visited.add(name)
            total += size
            records.append(dict(name=name, unit=owner["unit"], unit_offset=owner["start"],
                                size=size, normalized_sha256=hashlib.sha256(normalized_unit).hexdigest(),
                                direct=direct, indirect=indirect))
    certificate = dict(domain="nelisp-rooted-direct-text-v1", roots=roots,
                       records=records, total_bytes=total,
                       status="DIRECT_TEXT_CLOSURE_PASS", runtime_memory="pending",
                       indirect_semantics="Not certified by direct text ownership")
    if claimed is not None and claimed != certificate:
        raise ValueError("Claimed proof drops or changes decoded ownership")
    return certificate


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--image", type=Path, required=True)
    parser.add_argument("--metadata", type=Path, required=True)
    parser.add_argument("--unit-directory", type=Path, required=True)
    parser.add_argument("--root", action="append", required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    result = prove(args.image, args.metadata, args.unit_directory, args.root)
    args.output.write_text(json.dumps(result, indent=2) + "\n")
    print("DIRECT_TEXT_CLOSURE_PASS functions=%d bytes=%d" %
          (len(result["records"]), result["total_bytes"]))
