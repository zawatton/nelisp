#!/usr/bin/env python3
"""Read-only ELF entry-exit evidence; no disassembler, build or mutations."""
import hashlib
import json
from pathlib import Path
import struct

ROOT = Path(__file__).resolve().parent
RT = ROOT.parent.parent/'ccore-runtime-20261002'
BINARY = RT/'target/nelisp-ccore-final'
b = BINARY.read_bytes()
assert b[:6] == b'\x7fELF\x02\x01', 'expected ELF64 little endian'
entry, phoff = struct.unpack_from('<QQ', b, 24)
phsize, phnum = struct.unpack_from('<HH', b, 54)
offset = None
for i in range(phnum):
    typ, flags, off, va, _, filesz, _, _ = struct.unpack_from('<IIQQQQQQ', b, phoff+i*phsize)
    if typ == 1 and va <= entry < va+filesz:
        offset = off+entry-va
        break
assert offset is not None
epilogue = b[offset+65:offset+74]
assert epilogue == bytes.fromhex('89c7b83c0000000f05'), epilogue.hex()
source = RT/'scripts/nelisp-standalone-build.el'
lines = source.read_text().splitlines()
matches = [{'line':i,'text':s.strip()} for i,s in enumerate(lines,1)
           if '(defun nl_os_exit_process (code) (syscall-direct 60' in s or '; mov eax, 60' in s]
result = dict(binary=str(BINARY),sha256=hashlib.sha256(b).hexdigest(),entry=hex(entry),
              entry_file_offset=hex(offset),epilogue_offset=65,bytes=epilogue.hex(),
              instructions=['mov edi,eax','mov eax,60','syscall'],
              interpretation='Linux SYS_exit is thread-only, not process-wide SYS_exit_group231',
              source=str(source),source_sha256=hashlib.sha256(source.read_bytes()).hexdigest(),matches=matches)
(ROOT/'results/exit.json').write_text(json.dumps(result,indent=2)+'\n')
print('EXIT-ENTRY-THREAD-ONLY', result['entry'],epilogue.hex())
