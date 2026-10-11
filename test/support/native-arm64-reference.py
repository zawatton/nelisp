#!/usr/bin/env python3
"""Generate a pinned AArch64 ELF encoding reference with LLVM's MC assembler.

Uses the public LLVM C API, independent of the NeLisp encoder.  No clang
driver or standalone reader build is required.  Prints a small JSON receipt.
"""
import ctypes as C
from ctypes.util import find_library
import hashlib
import json
from pathlib import Path
import struct
import sys

library = find_library("LLVM")
if not library:
    raise RuntimeError("LLVM shared library unavailable")
llvm = C.CDLL(library)


def function(name, result, *args):
    fn = getattr(llvm, name)
    fn.restype, fn.argtypes = result, args
    return fn


u32 = C.c_uint
ptr = C.c_void_p
outptr = C.POINTER(ptr)
version = [u32(), u32(), u32()]
function("LLVMGetVersion", None, *([C.POINTER(u32)] * 3))(*(C.byref(v) for v in version))
version_text = ".".join(str(v.value) for v in version)
if version[0].value != 19:
    raise RuntimeError(f"Expected LLVM 19 MC reference, got {version_text}")
for part in ("TargetInfo", "Target", "TargetMC", "AsmPrinter", "AsmParser"):
    function("LLVMInitializeAArch64" + part, None)()
context = function("LLVMContextCreate", ptr)()
module = function("LLVMModuleCreateWithNameInContext", ptr, C.c_char_p, ptr)(b"arm64-reference", context)
triple = b"aarch64-unknown-linux-gnu"
function("LLVMSetTarget", None, ptr, C.c_char_p)(module, triple)
source = Path(sys.argv[1]).read_bytes()
function("LLVMSetModuleInlineAsm2", None, ptr, C.c_char_p, C.c_size_t)(module, source, len(source))
target, error = ptr(), ptr()
if function("LLVMGetTargetFromTriple", C.c_int, C.c_char_p, outptr, outptr)(triple, C.byref(target), C.byref(error)):
    raise RuntimeError(C.string_at(error).decode())
machine = function("LLVMCreateTargetMachine", ptr, ptr, C.c_char_p, C.c_char_p, C.c_char_p,
                   C.c_int, C.c_int, C.c_int)(target, triple, b"generic", b"", 0, 0, 0)
buffer = ptr()
if function("LLVMTargetMachineEmitToMemoryBuffer", C.c_int, ptr, ptr, C.c_int, outptr, outptr)(
        machine, module, 1, C.byref(error), C.byref(buffer)):
    raise RuntimeError(C.string_at(error).decode())
start = function("LLVMGetBufferStart", ptr, ptr)(buffer)
size = function("LLVMGetBufferSize", C.c_size_t, ptr)(buffer)
obj = C.string_at(start, size)
if obj[:6] != b"\x7fELF\x02\x01" or struct.unpack_from("<H", obj, 18)[0] != 183:
    raise ValueError("Not an AArch64 little endian ELF64 object")
shoff = struct.unpack_from("<Q", obj, 40)[0]
entsize, count, names_index = struct.unpack_from("<HHH", obj, 58)
names_offset, names_size = struct.unpack_from("<QQ", obj, shoff + names_index*entsize + 24)
names = obj[names_offset:names_offset+names_size]
text = None
for i in range(count):
    entry = shoff + i*entsize
    name_index = struct.unpack_from("<I", obj, entry)[0]
    if names[name_index:].split(b"\0", 1)[0] == b".text":
        offset, length = struct.unpack_from("<QQ", obj, entry+24)
        text = obj[offset:offset+length]
if text is None:
    raise ValueError("Reference object has no .text")
json.dump({"llvm": version_text, "triple": triple.decode(),
           "source_sha256": hashlib.sha256(source).hexdigest(),
           "text_sha256": hashlib.sha256(text).hexdigest(), "text_hex": text.hex()}, sys.stdout, indent=2)
print()
function("LLVMDisposeMemoryBuffer", None, ptr)(buffer)
function("LLVMDisposeTargetMachine", None, ptr)(machine)
function("LLVMDisposeModule", None, ptr)(module)
function("LLVMContextDispose", None, ptr)(context)
