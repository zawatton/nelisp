#!/usr/bin/env python3
"""A legacy status finish must preserve its already-written physical OUT."""
from pathlib import Path
import os,subprocess,sys,tempfile
ROOT=Path(__file__).resolve().parents[1]
binary=Path(sys.argv[1]).resolve()
symbols={p[2]:int(p[0],16) for l in subprocess.check_output(['nm','-n',str(binary)],text=True).splitlines() if len(p:=l.split())==3 and p[1] in ('t','T')}
if 'nl_env_lookup_val_done' not in symbols:
 print('legacy env finish is not exported by this binary');sys.exit(2)
parent=ROOT/'target/abi-env-finish';parent.mkdir(exist_ok=True)
directory=Path(tempfile.mkdtemp(dir=parent))
script=directory/'probe.el'
script.write_text('(setq abi-env-finish %d)\n'%symbols['nl_env_lookup_val_done']+r'''
(let ((env (nelisp--native-env)) (checked 0))
 (dolist (case '((0 (0 0 0 0 0 0 0 0)) (1 (0 0 0 0 0 0 0 0))
                 (2 (0 0 0 0 0 0 0 0)) (2 (249 255 255 255 255 255 255 255))
                 (2 (255 255 255 255 255 255 255 127)) (2 (0 0 0 0 0 0 0 128))
                 (marker nil)))
  (dolist (status '(0 1))
   (let ((out (alloc-bytes 32 8)) (before nil))
    (dotimes (i 32) (ptr-write-u8 out i 0))
    (if (eq (car case) 'marker)
      (dotimes (i 32) (ptr-write-u8 out i (ptr-read-u8 (+ env 64) i)))
     (ptr-write-u8 out 0 (car case))
     (dotimes (i 8) (ptr-write-u8 out (+ i 8) (nth i (nth 1 case)))))
    (dotimes (i 32) (push (ptr-read-u8 out i) before))
    (let ((rc (ptr-call abi-env-finish status env out 0 0 0)))
     (unless (= rc (if (or (= status 1) (eq (car case) 'marker)) 1 0))
       (error "legacy finish status changed: %S/%d" case status)))
    (dotimes (i 32)
     (unless (= (ptr-read-u8 out i) (nth (- 31 i) before))
       (error "legacy finish changed OUT bytes: %S/%d byte %d" case status i)))
    (setq checked (+ checked 1)))))
 (princ (format "ABI-ENV-FINISH checked=%d PASS\n" checked)))
''')
p=subprocess.run([str(binary),'--load',str(script)],cwd=ROOT,env=dict(os.environ,TMPDIR=str(directory),XDG_CACHE_HOME=str(directory/'cache')),capture_output=True,timeout=120)
(directory/'stdout').write_bytes(p.stdout);(directory/'stderr').write_bytes(p.stderr)
passed=p.returncode==0 and not p.stderr and b'ABI-ENV-FINISH checked=14 PASS' in p.stdout
print('legacy env finish','PASS' if passed else 'FAIL',directory)
if not passed:print(p.returncode,p.stderr.decode()[-1500:])
sys.exit(0 if passed else 1)
