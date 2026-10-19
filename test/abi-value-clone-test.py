#!/usr/bin/env python3
"""Exercise physical-slot ownership and immediate WORDs through the C ABI."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
binary = Path(sys.argv[1]).resolve()
symbols = {p[2]: int(p[0], 16) for line in subprocess.check_output(
    ['nm', '-n', str(binary)], text=True).splitlines()
    if len(p := line.split()) == 3 and p[1] in ('t', 'T')}
parent = ROOT / 'target/abi-value-clone'
parent.mkdir(parents=True, exist_ok=True)
directory = Path(tempfile.mkdtemp(dir=parent))
library = directory / 'fixture.so'
source = directory / 'fixture.c'
source.write_text('#include <stdint.h>\n#include <string.h>\n' + '\n'.join(
    f'#define {name} ((int64_t (*)(int64_t,int64_t))(uintptr_t){symbols[symbol]}ULL)'
    for name, symbol in [('CLONE', 'nl_val_clone_into'), ('LOAD', 'nl_val_load')]) + r'''
int64_t abi_value_clone(int64_t unused) {
  _Alignas(8) unsigned char src[32], expected[32], scratch[32];
  int64_t dst, word;
  const unsigned char tags[] = {0,1,2,2,2,2,2,2};
  const uint64_t payloads[] = {0,0,0,UINT64_C(0xfffffffffffffff9),
    UINT64_C(0x1fffffffffffffff),UINT64_C(0xe000000000000000),
    UINT64_C(0x7fffffffffffffff),UINT64_C(0x8000000000000000)};
  for (int i=0; i<8; i++) {
    memset(src,0,32); src[0]=tags[i]; memcpy(src+8,&payloads[i],8);
    memcpy(expected,src,32); dst=0;
    word=CLONE((int64_t)(uintptr_t)src,(int64_t)(uintptr_t)&dst);
    if (!word || word!=dst || word==(int64_t)(uintptr_t)src) return 100+i;
    memset(src,0x55,32);  /* The caller may immediately reuse its view. */
    const void *view=(void *)(uintptr_t)LOAD(word,(int64_t)(uintptr_t)scratch);
    if (memcmp(view,expected,32)) return 200+i;
  }
  const int64_t words[]={3,7,1,-27,INT64_MAX-2};
  for (int i=0; i<5; i++) {
    dst=0;
    if (CLONE(words[i],(int64_t)(uintptr_t)&dst)!=words[i] || dst!=words[i]) return 300+i;
  }
  return 13;
}
''')
subprocess.run(['cc', '-shared', '-fPIC', '-o', str(library), str(source)], check=True)
probe = directory / 'probe.el'
probe.write_text(';;; -*- lexical-binding: t; -*-\n' + r'''
(defun abi-value-cstring (text)
  (let ((p (alloc-bytes (1+ (length text)) 1)))
    (ptr-write-bytes p text) (ptr-write-u8 p (length text) 0) p))
(let* ((handle (nl-ffi-call "dlopen" (abi-value-cstring LIBRARY) 2))
       (entry (nl-ffi-call "dlsym" handle (abi-value-cstring "abi_value_clone"))))
  (unless (> entry 0) (error "value clone fixture did not open"))
  (let ((result (ptr-call entry 0 0 0 0 0 0)))
    (unless (= result 13) (error "physical value clone changed: %d" result)))
  (princ "ABI-VALUE-CLONE checks=13 PASS\n"))
'''.replace('LIBRARY', json.dumps(str(library))))
result = subprocess.run([str(binary), '--load', str(probe)], cwd=ROOT,
                        env=dict(os.environ, TMPDIR=str(directory),
                                 XDG_CACHE_HOME=str(directory / 'cache')),
                        capture_output=True, timeout=120)
(directory / 'stdout').write_bytes(result.stdout)
(directory / 'stderr').write_bytes(result.stderr)
ok = result.returncode == 0 and not result.stderr and b'ABI-VALUE-CLONE checks=13 PASS' in result.stdout
print('physical/WORD clone13', 'PASS' if ok else 'FAIL', directory)
if not ok:
    print(result.returncode, result.stderr.decode()[:700])
sys.exit(0 if ok else 1)
