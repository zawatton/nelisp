#!/usr/bin/env python3
# Ground-truth probe #2: does the Apple Silicon kernel exec a HAND-BUILT,
# no-dyld LC_UNIXTHREAD arm64 Mach-O (raw svc syscall) after ad-hoc signing?
# The system LINKER refuses to make such a binary, but we emit bytes directly,
# so only the KERNEL's acceptance is in question. If `./e3` -> exit 42, the
# entire macOS port can skip dyld/libSystem/chained-fixups/symtab.
import struct, sys

PAGE = 0x4000                      # arm64 macOS page size = 16 KiB
VMBASE = 0x100000000               # __TEXT vmaddr (just above 4 GiB __PAGEZERO)
CODE_OFF = 0x300                   # file/vm offset of code (slack before it lets codesign insert LC_CODE_SIGNATURE)

# --- the program: mov x0,#42 ; mov x16,#1 (SYS_exit) ; svc #0x80 ---
code = struct.pack("<III", 0xD2800540, 0xD2800030, 0xD4001001)

# --- mach_header_64 ---
MH_MAGIC_64 = 0xFEEDFACF
CPU_ARM64   = 0x0100000C
MH_EXECUTE  = 0x2
MH_NOUNDEFS = 0x1

def seg64(name, vmaddr, vmsize, fileoff, filesize, prot, sects=b"", nsects=0):
    cmd, LC_SEGMENT_64 = b"", 0x19
    body = struct.pack("<16sQQQQiiII",
        name.encode().ljust(16, b"\0"), vmaddr, vmsize, fileoff, filesize,
        prot, prot, nsects, 0)            # maxprot=initprot=prot
    cmdsize = 8 + len(body) + len(sects)
    return struct.pack("<II", LC_SEGMENT_64, cmdsize) + body + sects

def sect64(sname, segname, addr, size, offset):
    return struct.pack("<16s16sQQIIIIIII4x",
        sname.encode().ljust(16, b"\0"), segname.encode().ljust(16, b"\0"),
        addr, size, offset, 2, 0, 0, 0x80000400, 0, 0)  # align 2^2, flags PURE|SOME_INSTRUCTIONS, res1/res2/res3

text_sect = sect64("__text", "__TEXT", VMBASE + CODE_OFF, len(code), CODE_OFF)

lc_pagezero = seg64("__PAGEZERO", 0, VMBASE, 0, 0, 0)
lc_text     = seg64("__TEXT", VMBASE, PAGE, 0, PAGE, 5, sects=text_sect, nsects=1)
lc_linkedit = seg64("__LINKEDIT", VMBASE + PAGE, PAGE, PAGE, 0, 1)

# LC_BUILD_VERSION (platform=1 macOS) — include to match modern expectations
LC_BUILD_VERSION = 0x32
lc_build = struct.pack("<IIIIII", LC_BUILD_VERSION, 24, 1, 0x000E0000, 0x000E0000, 0)  # minos/sdk 14.0

# LC_UNIXTHREAD arm64: flavor=6, count=68, state(272 bytes), pc at state offset 256
LC_UNIXTHREAD = 0x5
state = bytearray(272)
struct.pack_into("<Q", state, 256, VMBASE + CODE_OFF)   # __pc = entry
lc_thread = struct.pack("<IIII", LC_UNIXTHREAD, 8 + 8 + 272, 6, 68) + bytes(state)

cmds = [lc_pagezero, lc_text, lc_linkedit, lc_build, lc_thread]
ncmds = len(cmds)
sizeofcmds = sum(len(c) for c in cmds)
header = struct.pack("<IiiIIIII", MH_MAGIC_64, CPU_ARM64, 0, MH_EXECUTE,
                     ncmds, sizeofcmds, MH_NOUNDEFS, 0)

blob = header + b"".join(cmds)
assert len(blob) <= CODE_OFF, f"load cmds {len(blob)} overrun code @ {CODE_OFF}"
out = bytearray(PAGE)              # __TEXT filesize = one full page
out[:len(blob)] = blob
out[CODE_OFF:CODE_OFF+len(code)] = code
# file ends at PAGE; __LINKEDIT (fileoff=PAGE, filesize=0) is where codesign appends.

with open(sys.argv[1] if len(sys.argv) > 1 else "e3", "wb") as f:
    f.write(out)
print(f"wrote {sys.argv[1] if len(sys.argv)>1 else 'e3'}: {len(out)} bytes, "
      f"entry pc=0x{VMBASE+CODE_OFF:x}, {ncmds} load cmds, sizeofcmds={sizeofcmds}")
