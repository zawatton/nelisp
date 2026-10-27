#!/usr/bin/env python3
"""check-s4.py: the S4 evidence tables (target/layout/s4/{paired,matrix,festall}.txt) were all
produced from the lane binary recorded in S3.1 (target/layout/s3/build-a.txt "binary sha256 X"):
each must carry the line "lane binary sha256 X".  Exit 0 iff all three do."""
import os, re, sys
R = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
T = os.path.join(R, 'target/layout')
m = re.search(r'^binary sha256 ([0-9a-f]{64})$', open(os.path.join(T, 's3/build-a.txt')).read(), re.M)
if not m:
    sys.exit('S3.1 evidence has no binary sha256')
ok = True
for f in ('paired', 'matrix', 'festall'):
    p = os.path.join(T, f's4/{f}.txt')
    has = os.path.exists(p) and f'lane binary sha256 {m.group(1)}' in open(p).read()
    print(f'{f}: {"OK" if has else "MISSING"}')
    ok &= has
sys.exit(0 if ok else 1)
