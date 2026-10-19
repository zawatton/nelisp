#!/usr/bin/env python3
"""Check canonical declaration tails, including unibyte docstrings."""
from pathlib import Path
import os,subprocess,sys,tempfile
ROOT=Path(__file__).resolve().parents[1]
binary=Path(sys.argv[1]).resolve();old='--legacy' in sys.argv[2:]
symbols={p[2]:int(p[0],16) for l in subprocess.check_output(['nm','-n',str(binary)],text=True).splitlines() if len(p:=l.split())==3 and p[1] in ('t','T')}
names=['nelisp_cons_construct','nl_alloc_str','nl_alloc_unibyte_str','nl_cons_cdr_word','nl_cons_car_word','nl_val_load','nl_sf_decl_normalize_tail']
parent=ROOT/'target/abi-decl-tail';parent.mkdir(exist_ok=True)
d=Path(tempfile.mkdtemp(dir=parent));src=d/'probe.el'
src.write_text('(setq '+ ' '.join(f'abi-tail-{i} {symbols[n]}' for i,n in enumerate(names))+')\n'+r'''
(defun abi-tail-cons (a d)
 (let ((out (alloc-bytes 32 8))) (ptr-call abi-tail-0 a d out 0 0 0) out))
(let ((checked 0))
 (dolist (word '(3 7 1 -27))
  (let* ((view (alloc-bytes 32 8))
         (_ (ptr-call abi-tail-5 word view 0 0 0 0))
         (answer (ptr-call abi-tail-6 INPUT 0 0 0 0 0)))
   (unless (= answer word) (error "atomic declaration tail changed"))
   (setq checked (+ checked 1))))
 (dolist (ctor (list abi-tail-1 abi-tail-2))
  (let* ((bytes (alloc-bytes 3 1))
         (_ (ptr-write-u8 bytes 0 100)) (_ (ptr-write-u8 bytes 1 111))
         (_ (ptr-write-u8 bytes 2 99))
         (doc (alloc-bytes 32 8)) (_ (ptr-call ctor bytes 3 doc 0 0 0))
         (body (abi-tail-cons 69 3))
         (docs (abi-tail-cons doc body))
         (tail (abi-tail-cons 3 docs))
         (result (ptr-call abi-tail-6 tail 0 0 0 0 0))
         (stripped (ptr-call abi-tail-3 result 0 0 0 0 0)))
   (unless (= (ptr-call abi-tail-4 stripped 0 0 0 0 0) 69)
     (error "declaration docstring was not stripped"))
   (unless (= (ptr-call abi-tail-3 stripped 0 0 0 0 0) 3)
     (error "declaration body tail changed"))
   (setq checked (+ checked 2))))
 (princ (format "ABI-DECL-TAIL checked=%d PASS\n" checked)))
'''.replace('INPUT','view' if old else 'word'))
p=subprocess.run([str(binary),'--load',str(src)],cwd=ROOT,env=dict(os.environ,TMPDIR=str(d),XDG_CACHE_HOME=str(d/'cache')),capture_output=True,timeout=120)
(d/'stdout').write_bytes(p.stdout);(d/'stderr').write_bytes(p.stderr)
ok=p.returncode==0 and not p.stderr and b'ABI-DECL-TAIL checked=8 PASS' in p.stdout
print('declaration tail8', 'PASS' if ok else 'FAIL',d)
if not ok:print(p.returncode,p.stderr.decode()[-1500:])
sys.exit(0 if ok else 1)
