#!/usr/bin/env python3
"""Check physical char-table ABI bytes from a real native C caller."""
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
parent = ROOT / 'target/abi-char-table-legacy'
parent.mkdir(parents=True, exist_ok=True)
directory = Path(tempfile.mkdtemp(dir=parent))
library = directory / 'fixture.so'
source = directory / 'fixture.c'
entries = {'MARK': ('nl_root_mark', 'int64_t (*)(int64_t)'),
           'RESERVE': ('nl_root_reserve', 'int64_t (*)(int64_t)'),
           'RELEASE': ('nl_root_release', 'int64_t (*)(int64_t,int64_t)'),
           'CREATE': ('nl_char_table_alloc', 'int64_t (*)(int64_t,int64_t,int64_t)'),
           'GET': ('nl_char_table_get_raw', 'int64_t (*)(int64_t,int64_t,int64_t)'),
           'SET': ('nl_char_table_set_raw', 'int64_t (*)(int64_t,int64_t,int64_t,int64_t)')}
source.write_text('#include <stdint.h>\n#include <string.h>\n' + '\n'.join(
    f'#define {name} (({ctype}) (uintptr_t) {symbols[symbol]}ULL)'
    for name, (symbol, ctype) in entries.items()) + r'''
int64_t abi_char_legacy(int64_t env) {
  int64_t mark = MARK(env), table = RESERVE(env), value = RESERVE(env), out = RESERVE(env);
  int64_t result = 1;
  const unsigned char tags[] = {0,1,2,2,2,2};
  const uint64_t payloads[] = {0,0,0,UINT64_C(0xfffffffffffffff9),
                              UINT64_C(0x7fffffffffffffff),UINT64_C(0x8000000000000000)};
  memset((void *)(uintptr_t)value,0,32);
  if (CREATE(value,value,table)) result = 100;
  for (int i=0; i<6 && result==1; i++) {
    unsigned char *v=(void *)(uintptr_t)value, *o=(void *)(uintptr_t)out;
    memset(v,0,32); v[0]=tags[i]; memcpy(v+8,&payloads[i],8);
    if (SET(table,17,value,out)) result = 200+i;
    else if (GET(table,17,out)) result = 300+i;
    else if (memcmp(v,o,32)) result = 400+i;
  }
  RELEASE(env,mark);
  return result;
}
''')
subprocess.run(['cc', '-shared', '-fPIC', '-o', str(library), str(source)], check=True)
probe = directory / 'probe.el'
probe.write_text(';;; -*- lexical-binding: t; -*-\n' + r'''
(defun abi-char-cstring (text)
  (let ((p (alloc-bytes (1+ (length text)) 1)))
    (ptr-write-bytes p text) (ptr-write-u8 p (length text) 0) p))
(let* ((handle (nl-ffi-call "dlopen" (abi-char-cstring LIBRARY) 2))
       (entry (nl-ffi-call "dlsym" handle (abi-char-cstring "abi_char_legacy"))))
  (unless (> entry 0) (error "legacy fixture did not open"))
  (let ((result (ptr-call entry (nelisp--native-env) 0 0 0 0 0)))
    (unless (= result 1) (error "legacy char-table C ABI changed: %d" result)))
  (princ "ABI-CHAR-TABLE-LEGACY checked=6 PASS\n"))
'''.replace('LIBRARY', json.dumps(str(library))))
result = subprocess.run([str(binary), '--load', str(probe)], cwd=ROOT,
                        env=dict(os.environ, TMPDIR=str(directory),
                                 XDG_CACHE_HOME=str(directory / 'cache')),
                        capture_output=True, timeout=120)
(directory / 'stdout').write_bytes(result.stdout)
(directory / 'stderr').write_bytes(result.stderr)
ok = result.returncode == 0 and not result.stderr and b'ABI-CHAR-TABLE-LEGACY checked=6 PASS' in result.stdout
print('char-table legacy6', 'PASS' if ok else 'FAIL', directory)
if not ok:
    print(result.returncode, result.stderr.decode()[:700])
sys.exit(0 if ok else 1)
