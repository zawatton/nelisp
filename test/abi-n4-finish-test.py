#!/usr/bin/env python3
"""Legacy activation finish with no activation must preserve all OUT bytes."""
from pathlib import Path
import os,subprocess,sys,tempfile
ROOT=Path(__file__).resolve().parents[1];binary=Path(sys.argv[1]).resolve()
symbols={p[2]:int(p[0],16) for l in subprocess.check_output(['nm','-n',str(binary)],text=True).splitlines() if len(p:=l.split())==3 and p[1] in ('t','T')}
parent=ROOT/'target/abi-n4-finish';parent.mkdir(exist_ok=True)
d=Path(tempfile.mkdtemp(dir=parent));src=d/'probe.el'
src.write_text('(setq abi-finish %d)\n'%symbols['nl_n4_finish']+r'''
(let ((env (nelisp--native-env)) (checked 0))
 (dolist (case '((0 (0 0 0 0 0 0 0 0)) (1 (0 0 0 0 0 0 0 0))
                (2 (0 0 0 0 0 0 0 0)) (2 (249 255 255 255 255 255 255 255))
                (2 (255 255 255 255 255 255 255 127)) (2 (0 0 0 0 0 0 0 128))))
  (dolist (status '(0 1 2))
   (let ((out (alloc-bytes 32 8)) (i 0))
    (ptr-write-u64 out 0 (car case))
    (while (< i 8) (ptr-write-u8 out (+ i 8) (nth i (nth 1 case))) (setq i (+ i 1)))
    (unless (= (ptr-call abi-finish status env out 0 0 0) status)
      (error "legacy finish status changed"))
    (unless (= (ptr-read-u8 out 0) (car case)) (error "legacy finish tag changed"))
    (setq i 0)
    (while (< i 8)
     (unless (= (ptr-read-u8 out (+ i 8)) (nth i (nth 1 case)))
       (error "legacy finish OUT byte changed: %S byte %d" case i))
     (setq i (+ i 1)))
    (setq checked (+ checked 1)))))
 (princ (format "ABI-N4-FINISH checked=%d PASS\n" checked)))
''')
p=subprocess.run([str(binary),'--load',str(src)],cwd=ROOT,env=dict(os.environ,TMPDIR=str(d),XDG_CACHE_HOME=str(d/'cache')),capture_output=True,timeout=120)
(d/'stdout').write_bytes(p.stdout);(d/'stderr').write_bytes(p.stderr)
ok=p.returncode==0 and not p.stderr and b'ABI-N4-FINISH checked=18 PASS' in p.stdout
print('activation finish18','PASS' if ok else 'FAIL',d)
if not ok:print(p.returncode,p.stderr.decode()[-1300:])
sys.exit(0 if ok else 1)
