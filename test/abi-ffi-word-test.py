#!/usr/bin/env python3
"""Exercise all 32 typed FFI signatures against a real local shared library."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]
binary = Path(sys.argv[1]).resolve()
parent = ROOT / "target/abi-ffi-word"
parent.mkdir(exist_ok=True)
directory = Path(tempfile.mkdtemp(dir=parent))
library = directory / "fixture.so"
source = ['#include <stdint.h>']
cases = []
for signature in range(32):
    arguments = [f'{"double" if i < 4 and signature & (1 << i) else "int64_t"} a{i}'
                 for i in range(6)]
    values = [(i + 1.5 if i < 4 and signature & (1 << i) else -i - 3)
              for i in range(6)]
    value = sum((i + 1) * x for i, x in enumerate(values)) - 100
    floating = bool(signature & 16)
    expected = float(value) if floating else int(value)
    result_type = 'double' if floating else 'int64_t'
    expression = ' + '.join(f'a{i} * {i + 1}.0' for i in range(6)) + ' - 100.0'
    source.append(f'{result_type} abi_ffi_{signature}({", ".join(arguments)}) '
                  f'{{ return ({result_type})({expression}); }}')
    cases.append(f'({signature} ({" ".join(map(str, values))}) {expected})')
source += ['int64_t abi_ffi_zero(void) { return 17; }',
           'int64_t abi_rooted_return(int64_t e, int64_t t, int64_t a, int64_t c, '
           'int64_t q, int64_t z) { return 512; }']
c_file = directory / 'fixture.c'
c_file.write_text('\n'.join(source) + '\n')
subprocess.run(['cc', '-shared', '-fPIC', '-o', str(library), str(c_file)], check=True)
script = directory / 'probe.el'
script.write_text(r'''
(defun abi-ffi-cstring (text)
  (let ((pointer (alloc-bytes (+ 1 (length text)) 1)))
    (ptr-write-bytes pointer text)
    (ptr-write-u8 pointer (length text) 0) pointer))
(let* ((handle (nl-ffi-call "dlopen" (abi-ffi-cstring LIBRARY) 2))
       (checked 0) (last-address 0))
  (unless (> handle 0) (error "FFI fixture did not open"))
  (dolist (case 'CASES)
    (let* ((signature (nth 0 case))
           (address (nl-ffi-call "dlsym" handle
                        (abi-ffi-cstring (format "abi_ffi_%d" signature)))))
      (unless (> address 0) (error "FFI fixture symbol missing"))
      (setq last-address address)
      (let ((result (apply #'ptr-call-typed address signature (nth 1 case))))
        (unless (and (= result (nth 2 case))
                     (if (>= signature 16) (floatp result) (integerp result)))
          (error "FFI WORD changed: signature %d result %S" signature result)))
      (setq checked (+ checked 1))))
  (unless (null (ptr-call-typed last-address 32 0 0 0 0 0 0))
    (error "unknown typed signature changed"))
  (setq checked (+ checked 1))
  (unless (and (= (nl-ffi-call "toupper" 97) 65)
               (= (nl-ffi-call "tolower" 90) 122)
               (= (nl-ffi-call "sqrt" 4.0) 2.0)
               (= (nl-ffi-call "pow" -2.0 3.0) -8.0)
               (null (nl-ffi-call "abi-no-such-foreign-function")))
    (error "fixed FFI dispatch changed"))
  (setq checked (+ checked 5))
  (let ((entry (nl-ffi-call "dlsym" handle (abi-ffi-cstring "abi_rooted_return"))))
    (dolist (case '((0 0 0 1 (0 . 0) ())
                    (1 2 2 3 (1 . 2) (17))
                    (1 -1 2 3 (1 . many) (17 18 19))))
      (let* ((descriptor (vector entry (nth 0 case) (nth 1 case)
                                 (nth 2 case) (nth 3 case) [] [] -1 nil))
             (fn (nelisp--native-subr-create descriptor 'abi-ffi-rooted 0 nil)))
        (unless (equal (nelisp--native-subr-arity fn) (nth 4 case))
          (error "rooted subr arity changed: %S" case))
        (unless (null (apply fn (nth 5 case))) (error "rooted fixture result changed"))
        (setq checked (+ checked 2)))))
  (unless (null (nelisp--native-subr-arity nil)) (error "non-subr arity changed"))
  (setq checked (+ checked 1))
  (garbage-collect)
  (unless (= (nl-ffi-call "dlclose" handle) 0) (error "FFI fixture did not close"))
  (setq checked (+ checked 1))
  (princ (format "ABI-FFI-WORD checked=%d PASS\n" checked)))
'''.replace('LIBRARY', json.dumps(str(library)))
 .replace('CASES', '(' + ' '.join(cases) + ')'))
result = subprocess.run([str(binary), '--load', str(script)], cwd=ROOT,
                        env=dict(os.environ, TMPDIR=str(directory),
                                 XDG_CACHE_HOME=str(directory / 'cache')),
                        capture_output=True, timeout=180)
(directory / 'stdout').write_bytes(result.stdout)
(directory / 'stderr').write_bytes(result.stderr)
passed = (result.returncode == 0 and not result.stderr
          and b'ABI-FFI-WORD checked=46 PASS\n' in result.stdout)
print(f"FFI WORD {'PASS' if passed else 'FAIL'}: {directory}")
if not passed:
    print(result.returncode, result.stderr.decode()[-2000:])
sys.exit(0 if passed else 1)
