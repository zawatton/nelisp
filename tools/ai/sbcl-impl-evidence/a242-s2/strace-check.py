#!/usr/bin/env python3
"""strace-check.py LOG IMGNAME MODE: MODE fast = an mmap of the image fd and no
read/pread64 >= 1 MiB from it; MODE reloc = a read/pread64 >= 1 MiB from it."""
import re, sys
log, img, mode = sys.argv[1:4]
fds, big_read, mapped = set(), 0, 0
for ln in open(log, errors="replace"):
    ln = re.sub(r"^\[pid +\d+\] |^\d+ +", "", ln)
    m = re.match(r'open(?:at)?\(.*"([^"]*)".*\) = (\d+)$', ln)
    if m and (m.group(1).endswith(img) or
              (m.group(1).startswith("/proc/self/fd/") and m.group(1)[14:] in fds)):
        fds.add(m.group(2)); continue
    m = re.match(r'close\((\d+)\)', ln)
    if m: fds.discard(m.group(1)); continue
    m = re.match(r'mmap\([^,]*, (\d+), [^,]*, [^,]*, (\d+), ', ln)
    if m and m.group(2) in fds: mapped += 1; continue
    m = re.match(r'(?:read|pread64)\((\d+), .*, (\d+)(?:, \d+)?\) = (\d+)$', ln)
    if m and m.group(1) in fds and int(m.group(3)) >= (1 << 20): big_read += 1
print(f"image fd mmaps {mapped}, reads >= 1 MiB {big_read}")
ok = (mapped > 0 and big_read == 0) if mode == "fast" else big_read > 0
sys.exit(0 if ok else 1)
