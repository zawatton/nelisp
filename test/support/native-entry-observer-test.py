#!/usr/bin/env python3
"""Execute the real entry observer under SysV and Win64 ABIs on a Linux host.
Set NATIVE_ENTRY_OBSERVER_SOURCE to an older source for a negative control.
This is a bounded host check; it does not qualify the Windows reader/trust path.
"""
import ctypes
import json
import mmap
import os
from pathlib import Path
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
GENERATOR = r''';;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require (quote json))
(load (getenv "NATIVE_ENTRY_OBSERVER_SOURCE") nil t t)
(dolist (windows '(nil t))
  (dolist (gc '(nil t))
    (let ((bytes nil) (writes nil)
          (nelisp-test-native-entry-gc-once gc)
          (nelisp-test-native-entry-constructor (lambda (&rest args) (car args))))
      (cl-letf (((symbol-function 'nelisp-native-load--windows-p) (lambda () windows))
                ((symbol-function 'nelisp-native-load-map-anonymous) (lambda (size _) (if (= size 4096) 2 1)))
                ((symbol-function 'nelisp-native-load--poke-string) (lambda (_addr _offset value) (setq bytes value)))
                ((symbol-function 'ptr-write-u64) (lambda (_addr offset value) (push (list offset value) writes)))
                ((symbol-function 'nelisp-native-load--symbol-addr) (lambda (_) 4))
                ((symbol-function 'nelisp-native-load--mprotect-rx) (lambda (&rest _) t)))
        (nelisp-test-native-entry-instrument [3 0 0 0 0 [] [] 0 nil] 'fixture 0 t))
      (let ((coding-system-for-write 'no-conversion))
        (write-region bytes nil (format (concat (getenv "OBSERVER_TEST_DIR") "/observer-%s-%s.bin") windows gc) nil 'silent))
      (with-temp-file (format (concat (getenv "OBSERVER_TEST_DIR") "/observer-%s-%s.json") windows gc)
        (insert (json-encode (vconcat (mapcar (function vconcat) writes))))))))
'''
HELPER = r'''#include <stdint.h>
#include <stddef.h>
#define MS __attribute__((ms_abi))
static uint64_t gc_calls;
uint64_t MS gc_ms(uint64_t ignored) { ++gc_calls; return ignored; }
uint64_t gc_sysv(uint64_t ignored) { ++gc_calls; return ignored; }
uint64_t MS entry_ms(uint64_t a,uint64_t b,uint64_t c,uint64_t d,uint64_t e,uint64_t f) { return a+2*b+3*c+4*d+5*e+6*f; }
uint64_t entry_sysv(uint64_t a,uint64_t b,uint64_t c,uint64_t d,uint64_t e,uint64_t f) { return a+2*b+3*c+4*d+5*e+6*f; }
uint64_t invoke(void *entry, int windows) {
 if(windows) return ((uint64_t (MS *)(uint64_t,uint64_t,uint64_t,uint64_t,uint64_t,uint64_t))entry)(11,22,33,44,55,66);
 return ((uint64_t (*)(uint64_t,uint64_t,uint64_t,uint64_t,uint64_t,uint64_t))entry)(11,22,33,44,55,66);
}
uint64_t collections(void) {return gc_calls;}
'''


def main():
    if os.name != 'posix' or not hasattr(mmap, 'PROT_EXEC'):
        raise SystemExit('This ABI check requires a POSIX x86-64 host and cc ms_abi support')
    with tempfile.TemporaryDirectory(prefix='nelisp-observer-') as directory:
        root = Path(directory)
        (root / 'generate.el').write_text(GENERATOR)
        (root / 'observer-host.c').write_text(HELPER)
        env = dict(os.environ, OBSERVER_TEST_DIR=str(root))
        env.setdefault('NATIVE_ENTRY_OBSERVER_SOURCE', str(ROOT / 'test/support/native-entry-observer.el'))
        for command in (
            [env.get('EMACS', 'emacs'), '-Q', '--batch', '-L', 'lisp', '-L', 'src', '-l', str(root / 'generate.el')],
            [env.get('CC', 'cc'), '-Wall', '-Wextra', '-Werror', '-shared', '-fPIC', str(root / 'observer-host.c'), '-o', str(root / 'observer-host.so')],
        ):
            result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True, text=True, timeout=30)
            if result.returncode:
                raise RuntimeError(result.stderr)
        lib=ctypes.CDLL(str(root/'observer-host.so'))
        lib.invoke.argtypes=[ctypes.c_void_p,ctypes.c_int];lib.invoke.restype=ctypes.c_uint64
        lib.collections.restype=ctypes.c_uint64
        for windows in (False,True):
         for gc in (False,True):
          stem=f'observer-{"t" if windows else "nil"}-{"t" if gc else "nil"}'
          data=(ctypes.c_uint64*(2+256*7))()
          entry=ctypes.cast(getattr(lib,'entry_ms' if windows else 'entry_sysv'),ctypes.c_void_p).value
          collector=ctypes.cast(getattr(lib,'gc_ms' if windows else 'gc_sysv'),ctypes.c_void_p).value
          buf=bytearray((root/(stem+'.bin')).read_bytes())
          for offset,value in json.loads((root/(stem+'.json')).read_text()):
           buf[offset:offset+8]={1:ctypes.addressof(data),3:entry,4:collector}[value].to_bytes(8,'little')
          code=mmap.mmap(-1,4096,prot=mmap.PROT_READ|mmap.PROT_WRITE|mmap.PROT_EXEC);code.write(buf)
          address=ctypes.addressof(ctypes.c_char.from_buffer(code));before=lib.collections()
          for iteration in range(2):
           assert lib.invoke(address,windows)==1001
           start=2+iteration*7
           assert list(data)[start:start+7]==[11,22,33,44,55,66,entry], (windows, gc, list(data)[start:start+7])
          assert data[0]==2 and data[1]==int(gc)
          assert lib.collections()-before==int(gc)
          print(f'OBSERVER-ABI-PASS windows={windows} gc={gc} entries=2')


if __name__ == "__main__":
    main()
