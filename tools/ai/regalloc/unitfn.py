#!/usr/bin/env python3
"""unitfn.py UNIT-FILE OUT-PREFIX NAME... : disassemble NAMEs from a build cache .unit file
(text base64 in :nelisp-cache-bytes-base64, or hex in older entries; symbols as (:name "N" :value OFF ...)); prints size,
instruction count, callee-saved pushes and frame-slot instructions per function."""
import base64, re, sys, subprocess
u, pre = sys.argv[1], sys.argv[2]
s = open(u, encoding='latin-1').read()
m = re.search(r'\(text :nelisp-cache-bytes-base64 "([A-Za-z0-9+/=]+)"', s)
if m:
    text = base64.b64decode(m.group(1))
else:
    text = bytes.fromhex(re.search(r'\(text :nelisp-cache-bytes-hex "([0-9a-f]+)"', s).group(1))
syms = sorted((int(v), n) for n, v in re.findall(r'\(:name "([^"]+)" :value (\d+) :size \d+ :section text', s))
for name in sys.argv[3:]:
    st = [v for v, n in syms if n == name][0]
    en = min([v for v, n in syms if v > st] + [len(text)])
    open(pre + name + '.bin', 'wb').write(text[st:en])
    d = subprocess.run(['objdump', '-D', '-b', 'binary', '-mi386:x86-64', '-M', 'intel', '--no-show-raw-insn', pre + name + '.bin'], capture_output=True, text=True).stdout
    open(pre + name + '.dis', 'w').write(d)
    ins = [l for l in d.splitlines() if re.match(r'\s+[0-9a-f]+:\t', l)]
    print(f"{name} size {en-st} insns {len(ins)} pushes {sum(1 for l in ins if re.search(r'push +r1[2-5]', l))} rbp {sum(1 for l in ins if 'rbp-0x' in l)}")
