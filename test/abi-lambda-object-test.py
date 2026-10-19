#!/usr/bin/env python3
"""Check the typed constructor status has only its proven heap-object caller."""
from pathlib import Path
import json,os,subprocess,sys,tempfile
ROOT=Path(__file__).resolve().parents[1]
binary=Path(sys.argv[1]).resolve();dump=Path(sys.argv[2])
units={d['unit']:d['source'] for l in dump.open() if (d:=json.loads(l))}
callers=[];defs={}
def walk(x,fn='-'):
 if not isinstance(x,list) or not x:return
 if x[0]=='defun':
  fn=x[1];defs[fn]=x
  for y in x[3:]:walk(y,fn)
  return
 if x[0]=='nl_abi_status_fail':callers.append(fn)
 if x[0]=='extern-call' and len(x)>1 and x[1]=='nl_abi_status_fail':callers.append(fn)
 for y in x:walk(y,fn)
for source in units.values():walk(source)
assert callers==['nl_sf_lambda_w'],callers
gate=defs['nl_abi_status_fail'];assert gate[3][0]=='if' and gate[3][2]==gate[2][1],gate
symbols={p[2]:int(p[0],16) for l in subprocess.check_output(['nm','-n',str(binary)],text=True).splitlines() if len(p:=l.split())==3 and p[1] in ('t','T')}
parent=ROOT/'target/abi-lambda-object';parent.mkdir(exist_ok=True)
directory=Path(tempfile.mkdtemp(dir=parent))
prefix='(setq abi-cons %d abi-lambda %d abi-lambda-old %d)\n'%tuple(symbols[x] for x in ('nelisp_cons_construct_w','nl_sf_lambda_w','nl_sf_lambda'))
script=directory/'probe.el'
script.write_text(prefix+r'''
(let ((env (nelisp--native-env)) (checked 0))
 (dolist (value '(3 7 1 -27))
  (let* ((body (ptr-call abi-cons value 3 0 0 0 0))
         (args (ptr-call abi-cons 3 body 0 0 0 0))
         (scratch (alloc-bytes 32 8)) (out (alloc-bytes 32 8))
         (word (ptr-call abi-lambda args env 0 scratch 0 0)))
   (unless (and (> word 8) (= (logand word 1) 0) (= (ptr-read-u8 word 0) 7))
     (error "lambda WORD is not its Cons object"))
   (unless (and (= (ptr-call abi-lambda-old args env out scratch 0 0) 0)
                (= (ptr-read-u8 out 0) 7))
     (error "legacy lambda did not produce a Cons"))
   (setq checked (+ checked 2))))
 (princ (format "ABI-LAMBDA-OBJECT checked=%d PASS\n" checked)))
''')
p=subprocess.run([str(binary),'--load',str(script)],cwd=ROOT,env=dict(os.environ,TMPDIR=str(directory),XDG_CACHE_HOME=str(directory/'cache')),capture_output=True,timeout=120)
(directory/'stdout').write_bytes(p.stdout);(directory/'stderr').write_bytes(p.stderr)
passed=p.returncode==0 and not p.stderr and b'ABI-LAMBDA-OBJECT checked=8 PASS' in p.stdout
print('typed lambda object','PASS' if passed else 'FAIL',directory)
if not passed:print(p.returncode,p.stderr.decode()[-1500:])
sys.exit(0 if passed else 1)
