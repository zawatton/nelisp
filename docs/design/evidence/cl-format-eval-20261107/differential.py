import sys,subprocess,json
R,N,P=sys.argv[1:4]
rows=[l.rstrip('\n').split('|') for l in open(R+'/corpus.txt') if l.strip()]
def arg_el(a):
    return {'foo':'"foo"','hi':'"hi"','LIST':"'(1 2 3)",'N':None}.get(a,a)
def arg_cl(a):
    return {'foo':'"foo"','hi':'"hi"','LIST':"'(1 2 3)",'N':None}.get(a,a)
el=["(dolist (r (list"]
for f,a in rows:
    x=arg_el(a); fs=f.replace('\\','\\\\').replace('"','\\"')
    el.append(f'(cons "{fs}" (lambda () (cl-format nil "{fs}"{" "+x if x else ""})))')
el.append(')) (princ (format "%S\\n" (cons (car r) (condition-case e (funcall (cdr r)) (error (list (quote ERR) (car e))))))))')
open(R+'/d.el','w').write(';;; -*- lexical-binding: t; -*-\n(require \'cl-format)\n'+'\n'.join(el))
cl=[]
for f,a in rows:
    x=arg_cl(a); fs=f.replace('\\','\\\\').replace('"','\\"')
    cl.append(f'(format t "~s~%" (substitute #\\| #\\Newline (handler-case (format nil "{fs}"{" "+x if x else ""}) (error () "ERR"))))')
open(R+'/d.lisp','w').write('\n'.join(cl))
run=lambda c:subprocess.run(['bash','-c','ulimit -v 8388608; exec "$@"','_']+c,capture_output=True,text=True,timeout=120,errors='replace').stdout
o=run([N,'-L',P,'--load',R+'/shim3.el','--load',R+'/d.el'])
seen=[];
for l in o.splitlines():
    if l.startswith('("') and l not in seen: seen.append(l)
import re
ne={}
for l in seen:
    m=re.match(r'\("((?:[^"\\]|\\.)*)" \. (.*)\)$',l,re.S)
    if m: ne[m.group(1).encode().decode('unicode_escape')]=m.group(2)
sb=run(['sbcl','--noinform','--no-userinit','--no-sysinit','--non-interactive','--load',R+'/d.lisp']).splitlines()
sb=[l for l in sb if l.startswith('"')]
same=0;diff=[]
for (f,a),s in zip(rows,sb):
    n=ne.get(f,'<none>').replace('\\n','|')
    if n==s: same+=1
    else: diff.append((f,a,s,n))
print(f'corpus={len(rows)} sbcl-rows={len(sb)} nelisp-rows={len(ne)} same={same} diff={len(diff)}')
for d in diff: print('DIFF %-22s arg=%-6s sbcl=%-26s nelisp=%s'%d)
