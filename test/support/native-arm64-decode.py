#!/usr/bin/env python3
"""Capstone 5.0.7 reference decoder for the shared AArch64 acceptance ERT.

JSON on stdin: {hex: TEXT, literal_spans: [[START, END], ...]}.
Literal ownership is explicit: never decode address data as instructions.
Exit 77 means the module is absent; a wrong version or bad decode is an error.
"""
import json
import sys

try:
    import capstone
except ModuleNotFoundError as exc:
    if exc.name != "capstone":
        raise
    sys.exit(77)

if capstone.__version__ != "5.0.7":
    raise RuntimeError(f"Expected Capstone 5.0.7, got {capstone.__version__}")
if sys.argv[1:] == ["--available"]:
    sys.exit(0)

request = json.load(sys.stdin)
code = bytes.fromhex(request["hex"])
if len(code) > 16 * 1024 * 1024 or len(code) % 4:
    raise ValueError("Unaligned or oversized text")
spans = sorted(request.get("literal_spans", []))
cursor = 0
regions = []
for start, end in spans:
    if not (cursor <= start < end <= len(code) and start % 8 == 0 and (end-start) % 8 == 0):
        raise ValueError("Invalid literal ownership")
    regions.append((cursor, start))
    cursor = end
regions.append((cursor, len(code)))
decoder = capstone.Cs(capstone.CS_ARCH_ARM64, capstone.CS_MODE_LITTLE_ENDIAN)
decoder.detail = True
rows = []
for start, end in regions:
    decoded = list(decoder.disasm(code[start:end], start))
    if sum(ins.size for ins in decoded) != end-start:
        raise ValueError(f"Incomplete decode at {start:#x}..{end:#x}")
    for ins in decoded:
        reads, writes = ins.regs_access()
        rows.append({"address": ins.address, "mnemonic": ins.mnemonic,
                     "operands": ins.op_str, "word": int.from_bytes(ins.bytes, "little"),
                     "registers": sorted({ins.reg_name(reg) for reg in reads+writes}),
                     "immediates": [op.imm for op in ins.operands if op.type == capstone.arm64.ARM64_OP_IMM]})
json.dump({"version": capstone.__version__, "instructions": rows}, sys.stdout)
