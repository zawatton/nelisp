#!/usr/bin/env python3
"""Execute actual emitted prologues and measure their per-call cost."""
import ctypes
import json
import mmap
import os
from pathlib import Path
import statistics
import sys
import time

directory = Path(sys.argv[1])
cpu = min(os.sched_getaffinity(0))
os.sched_setaffinity(0, {cpu})
buffers = []
functions = {}
# Redirect RSP only inside this call-free test shim, retaining the real stack
# in R8. Canaries model the excluded saved-register and odd parameter-pad
# sides of the rounded runtime region; the production loop changes neither.
for loop_file in ("loop.bin", "scalar-loop.bin"):
    loop = (directory / loop_file).read_bytes()
    shim_code = bytes.fromhex("49 89 e0 48 89 fc") + loop + bytes.fromhex("4c 89 c4 c3")
    shim_memory = mmap.mmap(-1, len(shim_code), prot=mmap.PROT_READ | mmap.PROT_WRITE | mmap.PROT_EXEC)
    shim_memory.write(shim_code)
    buffers.append(shim_memory)
    shim = ctypes.CFUNCTYPE(None, ctypes.c_void_p)(ctypes.addressof(ctypes.c_char.from_buffer(shim_memory)))
    region = (ctypes.c_uint64 * 2048)(*([0xAABBCCDDEEFF1122] * 2048))
    shim(ctypes.addressof(region) + 8192)
    assert list(region)[1024:1028] == [0, 0, 0, 0]
    assert region[1023] == region[1028] == 0xAABBCCDDEEFF1122, "frame boundary overwritten"
# Test defensive DF normalization entirely inside a native shim. Even a
# failing variant restores DF before it returns to Python/C.
rep_loop = (directory / "loop.bin").read_bytes()
df_shim_code = (bytes.fromhex("49 89 e0 48 89 fc fd") + rep_loop
                + bytes.fromhex("4c 89 c4 9c 58 48 c1 e8 0a 48 83 e0 01 fc c3"))
df_shim_memory = mmap.mmap(-1, len(df_shim_code), prot=mmap.PROT_READ | mmap.PROT_WRITE | mmap.PROT_EXEC)
df_shim_memory.write(df_shim_code)
buffers.append(df_shim_memory)
df_shim = ctypes.CFUNCTYPE(ctypes.c_uint64, ctypes.c_void_p)(ctypes.addressof(ctypes.c_char.from_buffer(df_shim_memory)))
region = (ctypes.c_uint64 * 2048)(*([0xAABBCCDDEEFF1122] * 2048))
assert df_shim(ctypes.addressof(region) + 8192) == 0
assert list(region)[1024:1028] == [0, 0, 0, 0]
assert region[1023] == region[1028] == 0xAABBCCDDEEFF1122
flags_code = bytes.fromhex("9c 58 48 c1 e8 0a 48 83 e0 01 c3")
flags_memory = mmap.mmap(-1, len(flags_code), prot=mmap.PROT_READ | mmap.PROT_WRITE | mmap.PROT_EXEC)
flags_memory.write(flags_code)
buffers.append(flags_memory)
direction_flag = ctypes.CFUNCTYPE(ctypes.c_uint64)(ctypes.addressof(ctypes.c_char.from_buffer(flags_memory)))
assert direction_flag() == 0, "SysV entry requires clear DF"
for name in ("control", "scalar", "initialized"):
    code = (directory / (name + ".bin")).read_bytes()
    memory = mmap.mmap(-1, len(code), prot=mmap.PROT_READ | mmap.PROT_WRITE | mmap.PROT_EXEC)
    memory.write(code)
    buffers.append(memory)
    pointer = ctypes.addressof(ctypes.c_char.from_buffer(memory))
    functions[name] = ctypes.CFUNCTYPE(ctypes.c_uint64, *([ctypes.c_uint64] * 4))(pointer)
    assert functions[name](1, 11, 22, 33) == 66, "argument spill corruption"
    value = functions[name](0, 11, 22, 33)
    assert value == (3735928559 if name == "control" else 0), (name, value)
    assert direction_flag() == 0, "SysV return requires clear DF"

samples = {name: [] for name in functions}
for _ in range(5):
    for name, function in functions.items():
        start = time.perf_counter_ns()
        for index in range(10000):
            function(0, 11, 22, 33)
        samples[name].append((time.perf_counter_ns() - start) / 10000)
print("FRESH-FRAME " + json.dumps({"rt_bytes": 59504,
      "qword_stores": 7438, "cpu": cpu, "sysv_df": 0,
      "boundary_canaries": "preserved", "samples_ns_per_call": samples,
      "median_ns_per_call": {name: statistics.median(values) for name, values in samples.items()},
      "scope": "emitted prologue microbenchmark; native builtin throughput remains unverified"}))
