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
scalar_values = [0, -7, 17, 2305843009213693951, -2305843009213693952]
for index, value in enumerate(scalar_values):
    source.append(f'int64_t abi_scalar{index}(void) {{ return (int64_t)((UINT64_C({value & ((1 << 64) - 1)}) << 2) | 2); }}')
symbols = {p[2]: int(p[0], 16) for line in subprocess.check_output(
    ['nm', '-n', str(binary)], text=True).splitlines()
    if len(p := line.split()) == 3 and p[1] in ('t', 'T')}
source += [f'#define SCALAR_WORD ((int64_t (*)(int64_t,int64_t,int64_t,int64_t)) {symbols["nl_apply_native_subr_w"]}ULL)',
           f'#define SCALAR_VIEW ((int64_t (*)(int64_t,int64_t,int64_t,int64_t)) {symbols["nl_apply_native_subr"]}ULL)',
           f'#define ROOTED_FRAME ((uint64_t (*)(uintptr_t,uintptr_t,int64_t,uintptr_t,int64_t)) {symbols["nl_apply_rooted_subr_frame_w"]}ULL)',
           'static int64_t (*scalar_callbacks[])(void) = {' + ','.join(f'abi_scalar{i}' for i in range(5)) + '};',
           'static int64_t scalar_values[] = {' + ','.join(str(v) for v in scalar_values) + '};', r'''
int64_t abi_scalar_boundary(int64_t env) {
  uint64_t header[6] = {18 | (4 << 8),3,3,3,1,3}, object[4] = {18,0,0,0};
  uint64_t *slots = header + 2;
  uint64_t args[4] = {0}, out[4] = {0};
  object[1] = (uintptr_t)header;
  for (int i=0; i<5; i++) {
    uint64_t expected = ((uint64_t)scalar_values[i] << 2) | 1;
    slots[2] = ((uintptr_t)scalar_callbacks[i] << 2) | 1;
    if ((uint64_t)SCALAR_WORD((uintptr_t)object,(uintptr_t)args,env,0) != expected) return -100-i;
    if (SCALAR_VIEW((uintptr_t)object,(uintptr_t)args,env,(uintptr_t)out) != 0 ||
        out[0] != 2 || out[1] != (uint64_t)scalar_values[i]) return -200-i;
  }
  return 10;
}
int64_t abi_rooted_metadata(uintptr_t env) {
  int64_t checked = 0;
  for (int boxed=0; boxed<2; boxed++) {
    for (int argc=0; argc<=3; argc += argc == 0 ? 1 : 2) {
      uint64_t empty_header[1] = {8};
      uint64_t empty[4] = {8,(uintptr_t)empty_header,0,0};
      int64_t values[8] = {(intptr_t)abi_rooted_return,argc,argc,argc,argc+1,0,0,-1};
      uint64_t integers[8][4] = {{0}}, slots[9] = {0};
      for (int i=0; i<8; i++) {
        integers[i][0] = 2; integers[i][1] = (uint64_t)values[i];
        slots[i] = boxed ? (uintptr_t)integers[i] : ((uint64_t)values[i] << 2) | 1;
      }
      slots[5] = slots[6] = (uintptr_t)empty; slots[8] = 3;
      uint64_t vector_header[10] = {8 | (9 << 8)};
      for (int i=0; i<9; i++) vector_header[i+1] = slots[i];
      uint64_t vector[4] = {8,(uintptr_t)vector_header,0,0};
      uint64_t record_header[6] = {18 | (4 << 8),3,3,(uintptr_t)vector,1,3};
      uint64_t record[4] = {18,(uintptr_t)record_header,0,0};
      uint64_t args[3][4] = {{2,17,0,0},{2,18,0,0},{2,19,0,0}};
      uint64_t result = ROOTED_FRAME((uintptr_t)record,(uintptr_t)args,argc,env,0);
      if (!result || (result & 3) || *(uint64_t*)(uintptr_t)result != 0)
        return -300 - boxed*10 - argc;
      checked++;
    }
  }
  return checked;
}''']
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
  (let* ((address (nl-ffi-call "dlsym" handle (abi-ffi-cstring "abi_scalar_boundary")))
         (count (ptr-call address (nelisp--native-env) 0 0 0 0 0)))
    (unless (= count 10) (error "scalar0 C ABI result changed: %S" count))
    (setq checked (+ checked count)))
  (let* ((address (nl-ffi-call "dlsym" handle (abi-ffi-cstring "abi_rooted_metadata")))
         (count (ptr-call address (nelisp--native-env) 0 0 0 0 0)))
    (unless (= count 6) (error "rooted metadata representation changed: %S" count))
    (setq checked (+ checked count)))
  (let ((entry (nl-ffi-call "dlsym" handle (abi-ffi-cstring "abi_rooted_return"))))
    (dolist (representation '(literal returned))
     (dolist (case '((0 0 0 1 (0 . 0) ())
                     (1 2 2 3 (1 . 2) (17))
                     (1 -1 2 3 (1 . many) (17 18 19))))
      (let* ((descriptor
              (if (eq representation 'returned)
                  (vector entry (nth 0 case) (nth 1 case)
                          (nth 2 case) (nth 3 case) [] [] -1 nil)
                (cond ((= (nth 0 case) 0) (vector entry 0 0 0 1 [] [] -1 nil))
                      ((= (nth 1 case) 2) (vector entry 1 2 2 3 [] [] -1 nil))
                      (t (vector entry 1 -1 2 3 [] [] -1 nil)))))
             (fn (nelisp--native-subr-create descriptor 'abi-ffi-rooted 0 nil)))
        (unless (equal (nelisp--native-subr-arity fn) (nth 4 case))
          (error "rooted subr arity changed: %S" case))
        (unless (null (apply fn (nth 5 case))) (error "rooted fixture result changed"))
        ;; Exercise the flat FUNCALL gateway as well as list-based APPLY.
        (unless (null (cond ((= (nth 0 case) 0) (funcall fn))
                            ((= (nth 1 case) 2) (funcall fn 17))
                            (t (funcall fn 17 18 19))))
          (error "rooted flat fixture result changed: %S" case))
        (setq checked (+ checked 3))))))
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
          and b'ABI-FFI-WORD checked=74 PASS\n' in result.stdout)
print(f"FFI WORD {'PASS' if passed else 'FAIL'}: {directory}")
if not passed:
    print(result.returncode, result.stderr.decode()[-2000:])
sys.exit(0 if passed else 1)
