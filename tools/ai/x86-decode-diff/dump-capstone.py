"""Dump capstone's view of every function range in an evidence directory.

Usage: dump-capstone.py EVIDENCE_DIR INSN_TSV RANGES_TSV

INSN_TSV columns: unit function address size control operand imm_offset
imm_size target mnemonic ("-" where not applicable; mnemonic only for
branches).  A "RANGE-MISMATCH" line is emitted when capstone's decoded size
total differs from the range size.  RANGES_TSV columns: unit-file unit
function start end (consumed by dump-elisp.el).
"""
import importlib.util
import json
import sys
from pathlib import Path

from capstone import Cs, CS_ARCH_X86, CS_MODE_64, CS_GRP_CALL, CS_GRP_JUMP
from capstone.x86 import X86_OP_IMM

ROOT = Path(__file__).resolve().parents[3]
_spec = importlib.util.spec_from_file_location(
    "rooted_unit_owner", ROOT / "scripts" / "nelisp-native-rooted-direct-closure.py")
_owner = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_owner)


def main(directory, insn_path, ranges_path):
    directory = Path(directory)
    metadata = directory / "active-unit-metadata.json"
    owners = _owner.unit_owners(metadata, directory)
    unit_files = {u["name"]: Path(u["path"]).name
                  for u in json.loads(metadata.read_bytes())}
    decoder = Cs(CS_ARCH_X86, CS_MODE_64)
    decoder.detail = True
    with open(insn_path, "w") as out, open(ranges_path, "w") as rng:
        for name, owner in owners.items():
            start, end = owner["start"], owner["end"]
            rng.write(f"{directory / unit_files[owner['unit']]}\t{owner['unit']}\t{name}\t{start}\t{end}\n")
            decoded = 0
            for ins in decoder.disasm(owner["text"][start:end], start):
                decoded += ins.size
                control = operand = imm_offset = imm_size = target = mnem = "-"
                if ins.group(CS_GRP_CALL) or ins.group(CS_GRP_JUMP):
                    control = "call" if ins.group(CS_GRP_CALL) else "jump"
                    if len(ins.operands) == 1 and ins.operands[0].type == X86_OP_IMM:
                        operand = "imm"
                        imm_offset, imm_size = ins.imm_offset, ins.imm_size
                        target = ins.operands[0].imm
                    else:
                        operand = "indirect"
                    mnem = ins.mnemonic
                out.write(f"{owner['unit']}\t{name}\t{ins.address}\t{ins.size}\t{control}\t"
                          f"{operand}\t{imm_offset}\t{imm_size}\t{target}\t{mnem}\n")
            if decoded != end - start:
                out.write(f"RANGE-MISMATCH\t{owner['unit']}\t{name}\t{decoded}\t{end - start}\n")


if __name__ == "__main__":
    main(*sys.argv[1:4])
