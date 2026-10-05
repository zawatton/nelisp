#!/usr/bin/env python3
"""Independent native layout/IEEE data verification from saved probe ABI contracts."""
import ctypes as C
import json
from pathlib import Path
import struct

class Iovec(C.Structure):
    _fields_ = [('base', C.c_void_p), ('length', C.c_size_t)]
class Request(C.Structure):
    _fields_ = [('count', C.c_size_t), ('extension', C.c_void_p), ('opcode', C.c_uint8), ('isvoid', C.c_uint8)]
class Glyph(C.Structure):
    _fields_ = [('index', C.c_ulong), ('x', C.c_double), ('y', C.c_double)]
class FfiType(C.Structure):
    _fields_ = [('size', C.c_size_t), ('alignment', C.c_ushort), ('type', C.c_ushort), ('elements', C.c_void_p)]
class FfiCif(C.Structure):
    _fields_ = [('abi', C.c_int), ('nargs', C.c_uint), ('argtypes', C.c_void_p), ('rtype', C.c_void_p), ('bytes', C.c_uint), ('flags', C.c_uint)]

result = {}
for cls,expected in [(Iovec,16),(Request,24),(Glyph,24),(FfiType,24),(FfiCif,32)]:
    assert C.sizeof(cls) == expected
    result[cls.__name__] = dict(size=C.sizeof(cls), offsets={name:getattr(cls,name).offset for name,_ in cls._fields_})
assert Glyph.x.offset == 8 and Glyph.y.offset == 16
assert FfiType.type.offset == 10 and FfiType.elements.offset == 16
result['20.0_binary64'] = dict(bits=struct.unpack('<Q',struct.pack('<d',20.0))[0],
                              halves=struct.unpack('<II',struct.pack('<d',20.0)))
Path('probes/results/layouts.json').write_text(json.dumps(result,indent=2)+'\n')
print('LAYOUTS-PASS checked=',len(result))
